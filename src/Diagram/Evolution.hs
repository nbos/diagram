{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution (module Diagram.Evolution) where

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import Data.Tuple.Extra
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Bit (Bit(..))
import qualified Data.Bit as BV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U

import qualified Streaming.Prelude as S

import Diagram.Primitive
import Diagram.Information

import Diagram.Joints (Joints)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import Diagram.String
import qualified Diagram.Doubly as D
import Diagram.ConstrIntervals (CIs(..), byHead,
                                CI(..), -- headIndex,headSymbol,
                                ciLength, tailSymbol) --tailIndex
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Util

--------------
-- MUTATION --
--------------

data Mutation = AddLeft  !Sym
              | AddRight !Sym
              | Add2     !Sym !Sym
              | DelLeft  !Sym
              | DelRight !Sym
              | Del2     !Sym !Sym
  deriving(Show,Eq,Ord)

data MutType = Add | Del

typeOfMut :: Mutation -> MutType
typeOfMut (AddLeft _)  = Add
typeOfMut (AddRight _) = Add
typeOfMut (Add2 _ _)   = Add
typeOfMut (DelLeft _)  = Del
typeOfMut (DelRight _) = Del
typeOfMut (Del2 _ _)   = Del

---------------------
-- EVOLUTION STATE --
---------------------

-- state of model  :: ST               = D0
-- across intro    :: (ST' - ST) = DD0 = D1
-- across mutation :: (D1' - D1) = DD1 = D2
-- mut. update     :: (D2' - D2) = DD2 = D3

--  o <---------> o        o <---------> o
--         ^                      ^
--         |                      |
--         | <------------------> |
--         |                      |
--         v                      v
--  o <---------> o        o <---------> o

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
-- | Evolution state of a JointType in a given string
data EvolutionState s = EvolutionState
  -- string (D0)
  { _string :: !(Doubly s) -- underlying string :: [N]Sym (readonly)
  , _symbolD0s :: !(U.Vector Count) -- symbol counts (readonly)
  -- , _jointD0 = 0 by def. (no joint count prior to intro)

  -- joint type (D1)
  , _jointType :: !JointType
  , _leftSyms  :: !(MV.MVector s SymEntry) -- :: [m]SymEntry
  , _rightSyms :: !(MV.MVector s SymEntry) -- :: [m]SymEntry
  , _constructed :: !(BV.MVector s Bit) -- :: [N-1]Bool, only s0s toggled
  , _symbolD1s :: !(IntMap Int) -- delta symbol count :: u0 U u1 -> dn
  , _jointD1 :: !Count -- joint count, popCount of constructed

  -- mutations (D2s)
  , _entries :: !(Map Mutation MutEntry) -- mut -> ddns
  , _byLoss :: !(Map Double Mutation) } -- ddi -> mut

-- SYM ENTRY (MEMBERSHIP, DEPS) --

data SymEntry = SymEntry
  { _isMember :: !Bool -- ^ True iff self is member of the union type
  , _coSymsIn :: !IntSet -- ^ Symbols that have a joint with
                         -- self and member of the co-union
  , _dependents :: !IntSet -- ^ CoSymsIn that have self as only coSymsIn
  , _coSymsOut :: !IntSet } -- ^ Symbols that have a joint with self and
                            -- *not* member of the co-union
  deriving (Show,Eq,Ord)

emptyIn :: SymEntry
emptyIn = SymEntry True IS.empty IS.empty IS.empty

emptyOut :: SymEntry
emptyOut = SymEntry False IS.empty IS.empty IS.empty

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the given joint in the type
mutsOf :: (Sym,SymEntry) -> (Sym,SymEntry) -> [Mutation]
mutsOf se0@(_, SymEntry mem0 _ _ _) se1@(_, SymEntry mem1 _ _ _)
  | mem0, mem1 = delMutsOf se0 se1
  | Just mut <- addMutOf se0 se1 = [mut]
  | otherwise = []

-- | Give the (possibly empty) set of available Del mutations that would
-- take the given joint out of the type (assumes it's in)
delMutsOf :: (Sym,SymEntry) -> (Sym,SymEntry) -> [Mutation]
delMutsOf (s0, SymEntry _ _ d0s _) (s1, SymEntry _ _ d1s _)
  | d0s == IS.singleton s1
  , d1s == IS.singleton s0 = [Del2 s0 s1]
  | otherwise = [ DelLeft s0 | IS.null d0s ]
                ++ [ DelRight s1 | IS.null d1s ]

-- | Give the (possibly missing) mutation that would make the given
-- joint member of the type (assumes it's not)
addMutOf :: (Sym,SymEntry) -> (Sym,SymEntry) -> Maybe Mutation
addMutOf (s0, SymEntry mem0 ic0s _ _) (s1, SymEntry mem1 ic1s _ _)
  | mem0 = Just $ AddRight s1 -- assert (not mem1)
  | mem1 = Just $ AddLeft s0  -- assert (not mem0)
  | IS.null ic0s && IS.null ic1s = Just $ Add2 s0 s1
  | otherwise = Nothing -- some other mut intros s0 or s1

-- MUT ENTRY (COUNTS, SITES) --

data MutEntry = MutEntry
  { _jointD2  :: !Int -- delta joint count (dnm)
  , _symbolD2 :: !(IntMap Int) -- delta delta string symbol counts (ddns)
  , _constrD2 :: !CIs } -- constr. sites
  deriving (Show,Eq)

makeLenses ''SymEntry
makeLenses ''MutEntry
makeLenses ''EvolutionState

-- LOSS ENTRY (COUNTS) --

data LossBooks s = LBs
  { _addLeft  :: !(IntMap        LossEntry)
  , _addRight :: !(IntMap        LossEntry)
  , _add2     :: !(Map (Sym,Sym) LossEntry)
  , _delLeft  :: !(IntMap        LossEntry)
  , _delRight :: !(IntMap        LossEntry)
  , _del2     :: !(Map (Sym,Sym) LossEntry)
  , _affected :: !(MV.MVector s (Set Mutation)) }

data LossEntry = LossEntry
  { _symbolD2IntervalsLoss :: !Double -- ils loss
  , _symbolD2Intervals :: !(IntMap (Count, Count)) -- ils :: s -> (before, after)
  , _jointCountDelta :: !Int } -- dnm

entryLoss :: Int -> Int -> Int -> Int -> LossEntry -> Double
entryLoss m bigN nm vm' (LossEntry sLossB _ dnm) = nLoss + sLoss + rLoss
  where
    nm' = nm + dnm
    nLoss = logFact (m + bigN - nm')
    sLoss = sLossA + sLossB
    sLossA = logFact nm'
    rLoss = fromIntegral nm' * ilog vm'

-- | Enumerate all available mutations with their loss. Unsorted.
evalLosses :: Int -> Int -> JointType -> Int -> LossBooks s ->
              [(Double, Mutation)]
evalLosses m bigN jt nm (LBs als ars a2s dls drs d2s _) =
  concat $ zipWith (fmap . first) lossFns mutEntries
  where
    JT u0 u1 = jt
    sz0 = UT.size u0
    sz1 = UT.size u1
    vm = sz0 * sz1

    lossFns :: [LossEntry -> Double]
    lossFns = entryLoss m bigN nm <$> vm's
    vm's = [ vm + sz1 -- addLeft
           , vm + sz0 -- addRight
           , vm + sz0 + sz1 -- add2
           , vm - sz1 -- delLeft
           , vm - sz0 -- delRight
           , vm - sz0 - sz1 ] :: [Int] -- del2

    mutEntries :: [[(LossEntry, Mutation)]]
    mutEntries = [ swap . first AddLeft   <$> IM.toList als
                 , swap . first AddRight  <$> IM.toList ars
                 , swap . first (uc Add2) <$>  M.toList a2s
                 , swap . first DelLeft   <$> IM.toList dls
                 , swap . first DelRight  <$> IM.toList drs
                 , swap . first (uc Del2) <$>  M.toList d2s ]

-------------
-- GETTERS --
-------------

-- | m
numSymbols :: Monad m => EvolutionT m Int
numSymbols = leftSyms `uses` MV.length

-- | N, bigN
stringLen :: Monad m => EvolutionT m Int
stringLen = constructed `uses` MU.length

-- | Retrieve the mutation with the minimal loss.
getMin :: Monad m => EvolutionT m (Double, Mutation)
getMin = uses byLoss M.findMin

-- | Compute the difference in information/code length incurred by the
-- introduction of the current joint type (i.e. no further mutation)
getD1Info :: Monad m => EvolutionT m Double
getD1Info = do
  m <- numSymbols
  bigN <- stringLen
  nm <- use jointD1

  ns <- use symbolD0s
  ils <- uses symbolD1s $
         IM.elems . IM.mapWithKey (\s dn -> toSnd (+dn) (ns U.! s))

  vm <- uses jointType JT.variety
  return $ d1Info m bigN nm ils vm

----------
-- INIT --
----------

initState :: PrimMonad m => Int -> Int -> Doubly (PrimState m) -> U.Vector Int ->
  Joints CIs -> (JointType, Joints CIs) -> m (EvolutionState (PrimState m))
initState m bigN str ns allCIs (jt, members) = do
  ---- string ----
  constr <- MU.new bigN
  forM_ (IM.elems runs) $ \(CI hd _ len _ _) -> do
    let constrlen = (len `div` 2) * 2
    iss <- S.toList_ $ S.take constrlen $ D.streamWithKeyFrom str hd
    forM_ (in2s iss) $ BV.flipBit constr . fst . fst -- ((i0,s0),(i1,s1))
  --

  (uLeft, uRight) <- mkSymEntries m allJoints jt

  ---- mutations ----
  allCIsByMutRef <- newPrimRef @_ @Boxed M.empty
  forM_ (M.toList allCIs) $ \((s0,s1),sites) -> do
    muts <- mutsOf <$> sequence (s0, MV.read uLeft s0)
                   <*> sequence (s1, MV.read uRight s1)
    forM_ muts $ \mut ->
      modifyPrimRef allCIsByMutRef $ M.insertWith (<>) mut sites

  allCIsByMut <- readPrimRef allCIsByMutRef
  mutEntries <- flip M.traverseWithKey allCIsByMut $ \mut sites ->
    case typeOfMut mut of
      Add -> pure $
        let jddns = snd $ CIs.join_ memCIs sites -- joint ddns
            ddnm = sum jddns `div` 2
            sddns = negate <$> jddns -- string ddns == - joint ddns
        in MutEntry ddnm sddns sites

      Del -> do
        let (CIs _ ddns h2ts _) = sites
        ddns' <- flip2 foldM ddns (IM.elems h2ts) $
          \dd (CI hd s0 len _ s1) -> do
            hdIsConstr <- unBit <$> MU.read constr hd
            return $ if hdIsConstr then dd
                     else decr s0 $ if odd len then dd
                                    else decr s1 dd
        let ddnm = sum ddns' `div` 2
        return $ MutEntry ddnm ddns' sites

  let mutsByLoss = M.fromList $
                   (eval &&& fst) <$> M.toList mutEntries
  return $
    EvolutionState str ns jt uLeft uRight constr dns nm mutEntries mutsByLoss

  where
    allJoints = M.keys allCIs

    memCIs@(CIs _ jns runs _) = foldr1 (<>) members
    nm = sum jns `div` 2
    dns = negate <$> jns
    eval = uncurry $ evalMutation m bigN ns jt nm dns

    -- decrement the count of a symbol by 1
    decr :: Sym -> IntMap Count -> IntMap Count
    decr = IM.alter (nothingIf (==0) . maybe (-1) (+(-1)))

    in2s :: [a] -> [(a,a)]
    in2s (a:b:rest) = (a,b):in2s rest
    in2s _ = []

-- WHERE --

-- | Given the number of symbols, a list of all joints and a joint type,
-- return the SymEntries of the left and right unions of the type
mkSymEntries :: PrimMonad m => Int -> [(Sym,Sym)] -> JointType ->
                m ( MV.MVector (PrimState m) SymEntry
                  , MV.MVector (PrimState m) SymEntry )
mkSymEntries m allJoints (JT u0 u1) = do
  uLeft  <- MV.replicate m emptyOut -- uLeft
  uRight <- MV.replicate m emptyOut -- uRight
  forM_ s0s $ flip (MV.write uLeft ) emptyIn
  forM_ s1s $ flip (MV.write uRight) emptyIn

  -- cosyms (in/out)
  forM_ allJoints $ \(s0,s1) -> do
    se0 <- MV.read uLeft s0
    se1 <- MV.read uRight s1
    MV.write uLeft s0 $ se0 & if se1^.isMember
      then coSymsIn %~ IS.insert s1
      else coSymsOut %~ IS.insert s1
    MV.write uRight s1 $ se1 & if se0^.isMember
      then coSymsIn %~ IS.insert s0
      else coSymsOut %~ IS.insert s0

  -- deps
  forM_ s0s $ \s0 -> do
    ic0s <- _coSymsIn <$> MV.read uLeft s0
    case IS.toList ic0s of
      [s1] -> MV.modify uRight (dependents %~ IS.insert s0) s1
      _else -> return ()

  forM_ s1s $ \s1 -> do
    ic1s <- _coSymsIn <$> MV.read uRight s1
    case IS.toList ic1s of
      [s0] -> MV.modify uLeft (dependents %~ IS.insert s1) s0
      _else -> return ()

  return (uLeft, uRight)
  where
    s0s = UT.toList u0 -- left member symbols
    s1s = UT.toList u1 -- right member symbols

-- | Compute the difference in delta-info of a mutation, given all
-- required params and the difference in params it would result in
evalMutation :: Int -> Int -> U.Vector Count -> JointType -> Int ->
                IntMap Count -> Mutation -> MutEntry -> Double
evalMutation m bigN ns jt nm dns mut (MutEntry ddnm ddns _) = loss
  where
    loss = d2Loss m bigN ils' nm' vm'
    nm' = nm + ddnm
    ils' = IM.elems $ flip2 IM.intersectionWithKey dns ddns $
      \s dn ddn -> toSnd (+ddn) ((ns U.! s) + dn) -- (ni',ni'')

    vm' = (vm +) $ case mut of
      AddLeft _  ->  sz1
      AddRight _ ->  sz0
      Add2 _ _   ->  sz0 + sz1
      DelLeft _  -> -sz1
      DelRight _ -> -sz0
      Del2 _ _   -> -sz0 - sz1

    JT u0 u1 = jt
    sz0 = UT.size u0
    sz1 = UT.size u1
    vm = sz0 * sz1

-- | Enumerate the indexes of all constructive joints that would get
-- deleted from a type given only the reference string (read only), the
-- boolean vector marking the construction sites of the type (read only)
-- and a set of construction intervals to be deleted (assumed to be
-- sub-intervals of the construction of the type).
sitesFromDelIls :: PrimMonad m =>
  Doubly (PrimState m) -> BV.MVector (PrimState m) Bit -> CIs -> m [Index]
sitesFromDelIls str constr delCIs = fmap concat $
  forM (CIs.toList delCIs) $ \(CI hd _ len _ _) -> do
  hdIsConstr <- BV.unBit <$> MU.read constr hd
  if len < 3 && hdIsConstr
    then return [hd] -- simple case
    else fmap everyOther $ S.toList_ $ -- general case
         (if hdIsConstr then id else S.drop 1) $
         S.take len $ D.streamKeysFrom str hd

-- | Enumerate the indexes of all constructive joints that would get
-- added to a type given only the reference string (read only), the
-- boolean vector marking the construction sites of the type (read only)
-- and a set of construction intervals to be added (assumed to be
-- disjoint from the constructive intervals of the type).
sitesFromAddIls :: PrimMonad m =>
  Doubly (PrimState m) -> BV.MVector (PrimState m) Bit -> CIs -> m [Index]
sitesFromAddIls str constr addCIs = fmap concat $
  forM (CIs.toList addCIs) $ \(CI hd _ len _ _) -> do
  phdIsConstr <- (D.prev str hd >>=) $ \case
    Nothing -> return False
    Just phd -> BV.unBit <$> MU.read constr phd
  if len < 3 then return [ hd | not phdIsConstr ] -- simple case
    else fmap everyOther $ S.toList_ $ -- general case
         (if phdIsConstr then S.drop 1 else id) $
         S.take len $ D.streamKeysFrom str hd

-- | Skip every second value, returning every other, starting with the
-- first.
everyOther :: [a] -> [a]
everyOther [] = []
everyOther [a] = [a]
everyOther (a:_:rest) = a:everyOther rest

symD2sFromDelIls :: PrimMonad m =>
  Doubly (PrimState m) -> BV.MVector (PrimState m) Bit -> CIs -> m (IntMap Count)
symD2sFromDelIls str constr (CIs _ ns0 h2ts _) = flip execStateT ns0 $
  forM (IM.elems h2ts) $ \(CI hd _ len _ stl) -> do
  hdIsConstr <- BV.unBit <$> MU.read constr hd
  unless hdIsConstr $ do -- parity rotates
    modify . decr =<< D.read str hd
    when (len > 2) $ do
      let tlIsConstr = odd len -- normally `even`, but hd isn't constr
      modify $ (if tlIsConstr then decr else incr) stl
  where
    -- decrement the count of a symbol by 1, assumes present
    decr :: Sym -> IntMap Count -> IntMap Count
    decr = IM.update (nothingIf (==0) . (+(-1)))

    -- increment the count of a symbol by 1, assumes positive
    incr :: Sym -> IntMap Count -> IntMap Count
    incr = flip (IM.insertWith (+)) 1

symD2sFromAddIls :: PrimMonad m =>
  Doubly (PrimState m) -> BV.MVector (PrimState m) Bit -> CIs -> m (IntMap Count)
symD2sFromAddIls str constr (CIs _ ns0 h2ts _) = flip execStateT ns0 $
  forM (IM.elems h2ts) $ \(CI hd _ len _ stl) -> do
  phdIsConstr <- (D.prev str hd >>=) $ \case
    Nothing -> return False
    Just phd -> BV.unBit <$> MU.read constr phd
  when phdIsConstr $ do -- parity rotates
    modify . decr =<< D.read str hd
    when (len > 2) $ do
      let tlIsConstr = odd len -- normally `even`, but hd isn't constr
      modify $ (if tlIsConstr then incr else decr) stl
  where
    -- decrement the count of a symbol by 1, assumes present
    decr :: Sym -> IntMap Count -> IntMap Count
    decr = IM.update (nothingIf (==0) . (+(-1)))

    -- increment the count of a symbol by 1, assumes positive
    incr :: Sym -> IntMap Count -> IntMap Count
    incr = flip (IM.insertWith (+)) 1

-- | (WARNING: struggle code+comments) Given the string, a type and its
-- constructive signal on the string (bool vector), and the constructive
-- intervals of a joint type, return the set of corrections on the
-- symCounts of the CIs associated with a mutation (add or del) required
-- to be added in order for it to match the actual change in symbol
-- counts produced by the mutation. Corrections are signed to be *added*
-- to the CIs.symCounts before they are subtracted (add) or added (del)
-- to the joint type's own CIs.symCounts.
d2CountCorrections :: forall m. PrimMonad m => Doubly (PrimState m) ->
  MV.MVector (PrimState m) SymEntry -> MV.MVector (PrimState m) SymEntry ->
  BV.MVector (PrimState m) Bit -> CIs -> m (Map Mutation (IntMap Int))
d2CountCorrections str uLeft uRight constrv cis = fmap clean $
  flip execStateT M.empty $ forM_ cis_ $ \ci -> do
  goDels ci -- decompose, treat all delMuts
  -- grab the largest chain possible, if first in the chain
  (lift (grabPrev ci) >>=) $ flip whenJust $ \case
    Nothing -> (lift (grabFwd ci) >>=) $ flip whenJust $ \(addMut, nexts) ->
      goAdd addMut (ci:|nexts)
    Just (addMut, prv) -> (lift (grabFwd ci) >>=) $ \case
      Nothing -> goAdd addMut (prv:|[ci])
      Just (addMut', nexts)
        | addMut == addMut' -> goAdd addMut (prv:|ci:nexts)
        | otherwise -> goAdd addMut (prv:|[ci])
                       >> goAdd addMut' (ci:|nexts)
  where
    clean = M.filter IM.null . fmap (IM.filter (==0))
    cis_ = IM.elems $ cis^.byHead

    goAdd :: Mutation -> NonEmpty CI -> StateT (Map Mutation (IntMap Int)) m ()
    goAdd addMut ils = modify $ M.insertWith (IM.unionWith (+)) addMut im'
      where
        newLen = sum ((^.ciLength) <$> ils) -- constituents lengths
                 - (length ils - 1) -- overlaps

        im = L.foldl' (flip f) IM.empty (NE.init ils)
        f ci | even (ci^.ciLength) = IM.insertWith (+) (ci^.tailSymbol) (-1)
             | otherwise = id

        last_ = NE.last ils
        im' = case compare (even newLen) (even $ last_^.ciLength) of
          LT -> IM.insertWith (+) (last_^.tailSymbol) (-1) im
          EQ -> im
          GT -> IM.insertWith (+) (last_^.tailSymbol) 1 im

    -- | Given a constructive interval of the joint type (in), count all
    -- the differences in symbol counts between the symCounts of the CIs
    -- for all joints removed by the same del-mutation and and the real
    -- difference in symCounts from applying those mutations.
    goDels :: CI -> StateT (Map Mutation (IntMap Int)) m ()
    goDels ci = mapM_ (uc $ flip go True) -- True == constr
              . M.toList . M.fromListWith (++)
              . reverse . ffmap (:[]) -- reverse to maintain order
              =<< lift (decomposeIn ci)
      where
        CI _ _ _ citl _ = ci

        go :: Mutation -> Bool -> [CI] ->
              StateT (Map Mutation (IntMap Int)) m ()
        go delMut = go_ where
          go_ _ [] = return ()
          go_ phase (CI hd shd len tl stl : rest) = do
            unless (tl == citl) $ dec stl -- tl

            outOfPhase <- (phase /=) <$> lift (constr hd)
            -- out of phase with super-CI means prev hd will be constr
            -- means hd will still be constr. so hd will not be docked
            when outOfPhase $ dec shd -- hd

            let phase' = phase /= odd len -- xor
            go_ phase' rest

          dec :: Sym -> StateT (Map Mutation (IntMap Int)) m ()
          dec s = modify $ M.insertWith (const $ IM.insertWith (+) s (-1))
                  delMut (IM.singleton s (-1))

    -- | Break a constructive interval of the joint type (in) into an
    -- ordered (by tail) list of its segments by mutation.
    decomposeIn :: CI -> m [(Mutation, CI)]
    decomposeIn ci@(CI hd _ 2 tl _) = (,ci) <<$>> delMutsOf' hd tl
    decomposeIn ci@(CI hd shd _ _ _) = CIs.extension str ci
                                       >>= go [] hd shd . tail
      where
        go mcis _ _ [] = return mcis
        go mcis i0 s0 ((i1,s1):rest) = do
          muts <- delMutsOf' s0 s1
          let (alive, ended) = L.partition (flip elem muts . fst) mcis
              started = (, CI i0 s0 2 i1 s1) <$>
                        filter (`notElem` (fst <$> mcis)) muts
              mcis' = (++ started) $ (<<$>> alive) $ \c ->
                c{ _ciLength = _ciLength c + 1
                 , _tailIndex = i1
                 , _tailSymbol = s1 }
          (ended ++) <$> go mcis' i1 s1 rest

    delMutsOf' :: Sym -> Sym -> m [Mutation]
    delMutsOf' s0 s1 = delMutsOf <$> sequence (s0, MV.read uLeft s0)
                                 <*> sequence (s1, MV.read uRight s1)

    constr :: Index -> m Bool
    constr = fmap BV.unBit . MU.read constrv

    -- | Because there is at most one mutation that will make an
    -- out-joint an in-joint, once we find the mutation from the
    -- breaking joint, we go as far as that mutation goes backwards and
    -- return the interval when it ends. Outer Maybe is to signals to
    -- skip the CI (case handled by getNexts) and inner Maybe signals
    -- whether or not there is a CI with a mutation before the provided
    -- CI.
    grabPrev :: CI -> m (Maybe (Maybe (Mutation, CI)))
    grabPrev (CI tl stl _ _ _) = (D.prev str tl >>=) $ \case
      Nothing -> return $ Just Nothing -- no prev symbol/interval
      Just ptl -> do
        sptl <- D.read str ptl
        (addMutOf' sptl stl >>=) $ \case
          Nothing -> return $ Just Nothing -- no mut
          Just mut -> (mut,) <<<$>>> go 2 ptl sptl
            where
              go !len hd shd = (D.prev str hd >>=) $ \case
                Nothing -> return $ Just $ Just ci -- hit start, end
                Just phd -> do
                  sphd <- D.read str phd
                  (member sphd shd >>=) $ \case
                    True -> return Nothing -- not first of a chain (grabFwd)
                    False -> (addMutOf' sphd shd >>=) $ \case
                      Just mut' | mut' == mut -> go (len+1) phd sphd
                      _else -> return $ Just $ Just ci -- end of interval
                where
                  ci = CI hd shd len tl stl

    addMutOf' :: Sym -> Sym -> m (Maybe Mutation)
    addMutOf' s0 s1 = addMutOf <$> sequence (s0, MV.read uLeft s0)
                               <*> sequence (s1, MV.read uRight s1)

    member s0 s1 = liftA2 (&&) (_isMember <$> MV.read uLeft s0)
                               (_isMember <$> MV.read uRight s1)

    -- | Because there is at most one mutation that will make an
    -- out-joint an in-joint, once we find the mutation from the next
    -- joint, we go as far as that mutation goes forwards. Since
    -- out-intervals of the same add-mutation could be separated by
    -- in-intervals, cascading a parity change along an arbitrary number
    -- of out-intervals of that add-mutation interspersed by
    -- in-intervals, we return a list of intervals (both in/out),
    -- starting with the given in-interval. Calling `getPrev` on any
    -- in-interval stuck in such a sequence and not the first with
    -- return Nothing so that the interval sequence returned here only
    -- gets treated once.
    grabFwd :: CI -> m (Maybe (Mutation, [CI]))
    grabFwd ci@(CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
      Nothing -> return Nothing -- hit end
      Just i1 -> do
        s1 <- D.read str i1
        (addMutOf' s0 s1 >>=) $ \case
          Nothing -> return Nothing
          Just addMut -> Just . (addMut,) <$> grabOut [ci] (CI s0 i0) 2 i1 s1
            where
              grabOut :: [CI] -> (Len -> Index -> Sym -> CI) ->
                         Len -> Index -> Sym -> m [CI]
              grabOut acc mkCI !len tl stl = (D.next str tl >>=) $ \case
                Nothing -> return $ reverse acc' -- hit end of string
                Just ntl -> do
                  sntl <- D.read str ntl
                  (member stl sntl >>=) $ \case
                    True -> grabIn acc' (CI tl stl) 2 ntl sntl -- switch
                    False -> (addMutOf' stl sntl >>=) $ \case
                      Just addMut' | addMut' == addMut ->
                        grabOut acc mkCI (len+1) ntl sntl -- keep going
                      _else -> return $ reverse acc' -- end of intervals
                where
                  acc' = mkCI len tl stl : acc

              grabIn :: [CI] -> (Len -> Index -> Sym -> CI) ->
                        Len -> Index -> Sym -> m [CI]
              grabIn acc mkCI !len tl stl = (D.next str tl >>=) $ \case
                Nothing -> return $ reverse acc' -- hit end of string
                Just ntl -> do
                  sntl <- D.read str ntl
                  (member stl sntl >>=) $ \case
                    True -> grabIn acc mkCI (len+1) ntl sntl -- keep going
                    False -> (addMutOf' stl sntl >>=) $ \case
                      Just addMut' | addMut' == addMut ->
                        grabOut acc' (CI tl stl) 2 ntl sntl -- switch
                      _else -> return $ reverse acc' -- end of intervals
                where
                  acc' = mkCI len tl stl : acc

----------
-- MATH --
----------

-- | Loss function is the delta-delta-info (d2Info) of mutations without
-- the terms which are constant accross all mutations
d2Loss :: Int -> Int -> [(Int,Int)] -> Int -> Int -> Double
d2Loss m bigN ils' nm' vm' = nLoss + sLoss + rLoss
  where
    nLoss = logFact (m + bigN - nm')
    sLoss = logFact nm' + sum (uncurry (-) . both logFact <$> ils')
    rLoss = fromIntegral nm' * ilog vm'

-- | Return the difference between the deltaDeltaInfo and the mutLoss
-- (d2Loss). This is the sum of the terms of deltaDeltaInfo which are
-- constant across all mutations.
d2LossComplement :: Int -> Int -> Int -> Int -> Double
d2LossComplement m bigN nm vm = nLossC + sLossC + rLossC
  where
    nLossC = - logFact (m + bigN - nm)
    sLossC = logFact nm
    rLossC = - (fromIntegral nm * ilog vm)

-- | Compute the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
d2Info :: Int -> Int -> [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Double
d2Info m bigN ils' (nm,nm') (vm,vm') = nDeltaDelta
                                       + sDeltaDelta
                                       + rDeltaDelta
  where
    mpN = m + bigN
    nDeltaDelta = logFact (mpN - nm') - logFact (mpN - nm)

    sDeltaDelta = logFact nm - logFact nm'
                  + sum (uncurry (-) . both logFact <$> ils')
                  -- (`logFact ni` (old symbol count) cancel out)

    rDeltaDelta = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- | Compute the info delta from the introduction of a joint type given
-- parameters
d1Info :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Double
d1Info m bigN nm ils vm = nDelta + sDelta + rDelta
  where
    mpN = m + bigN
    nDelta = logFact (mpN - nm) - ilog m - logFact (mpN - 1)

    sDelta = sum (uncurry (-) . both logFact <$> ils)
             - logFact nm

    rDelta = fromIntegral nm * ilog vm

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral
