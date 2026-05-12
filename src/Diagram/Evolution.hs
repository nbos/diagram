{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Diagram.Evolution (module Diagram.Evolution) where

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict (StateT(..))

import Data.Tuple.Extra

import Data.Strict (type (:!:), Pair((:!:)))
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
import Diagram.Sites (Sites(..))
import qualified Diagram.Sites as Sites

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

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
-- | Evolution state of a JointType in a given string
data EvolutionState s = EvolutionState
  -- string
  { _string :: !(Doubly s) -- underlying string :: [N]Sym (readonly)
  , _symbolCounts :: !(U.Vector Count) -- (readonly)

  -- joint type
  , _jointType :: !JointType
  , _leftSyms  :: !(MV.MVector s SymState) -- :: [m]SymState
  , _rightSyms :: !(MV.MVector s SymState) -- :: [m]SymState

  , _constructed :: !(BV.MVector s Bit) -- :: [N]Bool, only toggle s0s
  , _jointCount :: !Count -- constructed popCount
  , _deltaSymbolCounts :: !(IntMap Int) -- dns :: u0 U u1 -> dn

  -- mutations
  , _entries :: !(Map Mutation MutEntry) -- mut -> ddns
  , _byLoss :: !(Map Double Mutation) } -- ddi -> mut

-- SYMBOL INFO (MEMBERSHIP) --

data SymState = SymState
  { _member :: !Bool -- ^ True iff self is member of the union type
  , _coSymsIn :: !(Count :!: IntSet) -- ^ Symbols that have a joint with
                                     -- self and member of the co-union
  , _dependents :: !IntSet -- ^ CoSymsIn that have self as only coSymsIn
  , _coSymsOut :: !IntSet } -- ^ Symbols that have a joint with self and
                            -- *not* member of the co-union
  deriving (Show,Eq,Ord)

emptyIn :: SymState
emptyIn = SymState True (0 :!: IS.empty) IS.empty IS.empty

emptyOut :: SymState
emptyOut = SymState False (0 :!: IS.empty) IS.empty IS.empty

-- MUTATION INFO --

data MutEntry = MutEntry
  { _ddJointCount :: !Int -- delta delta nm
  , _ddSymbolCounts :: !(IntMap Int) -- delta delta string ns
  , _ddSites :: !Sites } -- constr. sites
  deriving (Show,Eq)

makeLenses ''SymState
makeLenses ''MutEntry
makeLenses ''EvolutionState

-- MUTATIONS BY LOSS --

type ByCount  = IntMap
type ByLoss   = Map Double
type SymSet   = IntSet
type JointSet = Set (Int,Int)

data LossBooks s = LBs
  { _addLeft  :: !(IntMap        LossEntry)
  , _addRight :: !(IntMap        LossEntry)
  , _add2     :: !(Map (Sym,Sym) LossEntry)
  , _delLeft  :: !(IntMap        LossEntry)
  , _delRight :: !(IntMap        LossEntry)
  , _del2     :: !(Map (Sym,Sym) LossEntry)
  , _affected :: !(MV.MVector s (Set Mutation)) }

data LossEntry = LossEntry
  { _countIntervalsLoss :: !Double -- ils loss
  , _countIntervals :: !(IntMap (Count, Count)) -- ils :: s -> (before, after)
  , _deltaJointCount :: !Int } -- dnm

getLoss :: Int -> Int -> Int -> Int -> LossEntry -> Double
getLoss m bigN nm vm' (LossEntry sLossB _ dnm) = nLoss + sLoss + rLoss
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

    lossFns = getLoss m bigN nm <$> vm's
    vm's = [ vm + sz1 -- addLeft
           , vm + sz0 -- addRight
           , vm + sz0 + sz1 -- add2
           , vm - sz1 -- delLeft
           , vm - sz0 -- delRight
           , vm - sz0 - sz1 ] -- del2

    mutEntries = [ swap . first AddLeft   <$> IM.toList als
                 , swap . first AddRight  <$> IM.toList ars
                 , swap . first (uc Add2) <$>  M.toList a2s
                 , swap . first DelLeft   <$> IM.toList dls
                 , swap . first DelRight  <$> IM.toList drs
                 , swap . first (uc Del2) <$>  M.toList d2s ]
    uc = uncurry

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
getDeltaInfo :: Monad m => EvolutionT m Double
getDeltaInfo = do
  m <- numSymbols
  bigN <- stringLen
  nm <- use jointCount

  ns <- use symbolCounts
  ils <- uses deltaSymbolCounts $
         IM.elems . IM.mapWithKey (\s dn -> toSnd (+dn) (ns U.! s))

  vm <- uses jointType JT.variety
  return $ deltaInfo m bigN nm ils vm

----------
-- INIT --
----------

initState :: PrimMonad m => Int -> Int -> Doubly (PrimState m) -> U.Vector Int ->
  Joints Sites -> (JointType, Joints Sites) -> m (EvolutionState (PrimState m))
initState m bigN str ns allSites (jt@(JT u0 u1), members) = do
  ---- string ----
  constr <- MU.new bigN
  forM_ (IM.toList runs) $ \(hd, len :!: _tl) -> do
    let constrlen = (len `div` 2) * 2
    iss <- S.toList_ $ S.take constrlen $ D.streamWithKeyFrom str hd
    forM_ (in2s iss) $ BV.flipBit constr . fst . fst -- ((i0,s0),(i1,s1))
  --

  ---- joint type ----
  uLeft  <- MV.replicate m emptyOut -- uLeft
  uRight <- MV.replicate m emptyOut -- uRight
  forM_ s0s $ flip (MV.write uLeft ) emptyIn
  forM_ s1s $ flip (MV.write uRight) emptyIn

  -- cosyms (in/out)
  forM_ allJoints $ \(s0,s1) -> do
    sst0 <- MV.read uLeft s0
    sst1 <- MV.read uRight s1
    MV.write uLeft s0 $ sst0 & if sst1^.member
      then coSymsIn %~ ((+1) `bimap` IS.insert s1)
      else coSymsOut %~ IS.insert s1
    MV.write uRight s1 $ sst1 & if sst0^.member
      then coSymsIn %~ ((+1) `bimap` IS.insert s0)
      else coSymsOut %~ IS.insert s0

  -- deps
  forM_ s0s $ \s0 -> do
    nic0 :!: ic0s <- _coSymsIn <$> MV.read uLeft s0
    when (nic0 == 1) $ case IS.toList ic0s of
      [s1] -> MV.modify uRight (dependents %~ IS.insert s0) s1
      _else -> error $ "expected singleton, got: " ++ show _else

  forM_ s1s $ \s1 -> do
    nic1 :!: ic1s <- _coSymsIn <$> MV.read uRight s1
    when (nic1 == 1) $ case IS.toList ic1s of
      [s0] -> MV.modify uLeft (dependents %~ IS.insert s1) s0
      _else -> error $ "expected singleton, got: " ++ show _else
  --

  ---- mutations ----
  allSitesByMutRef <- newPrimRef @_ @Boxed M.empty
  forM_ (M.toList allSites) $ \(s0s1@(s0,s1),sites) -> do
    muts <- mutsOf s0s1 <$> MV.read uLeft s0
                        <*> MV.read uRight s1
    forM_ muts $ \mut ->
      modifyPrimRef allSitesByMutRef $ M.insertWith (<>) mut sites

  allSitesByMut <- readPrimRef allSitesByMutRef
  mutEntries <- flip M.traverseWithKey allSitesByMut $ \mut sites ->
    case typeOfMut mut of
      Add -> pure $
        let jddns = snd $ Sites.join memSites sites -- joint ddns
            ddnm = sum jddns `div` 2
            sddns = negate <$> jddns -- string ddns == - joint ddns
        in MutEntry ddnm sddns sites

      Del -> do
        let (Sites ddns h2ts _) = sites
        ddns' <- flip2 foldM ddns (IM.toList h2ts) $
          \dd (hd, len :!: (_, s1)) -> do
            hdIsConstr <- unBit <$> MU.read constr hd
            if hdIsConstr then return dd else do
              s0 <- D.read str hd
              return $ decr s0 $ if odd len then dd else decr s1 dd
        let ddnm = sum ddns' `div` 2
        return $ MutEntry ddnm ddns' sites

  let mutsByLoss = M.fromList $
                   (eval &&& fst) <$> M.toList mutEntries
  return $
    EvolutionState str ns jt uLeft uRight constr nm dns mutEntries mutsByLoss

  where
    allJoints = M.keys allSites

    memSites@(Sites jns runs _) = foldr1 (<>) members
    nm = sum jns `div` 2
    dns = negate <$> jns
    eval = uncurry $ evalMutation m bigN ns jt nm dns

    -- decrement the count of a symbol by 1
    decr :: Sym -> IntMap Count -> IntMap Count
    decr = IM.alter (nothingIf (==0) . maybe (-1) (+(-1)))

    s0s = UT.toList u0 -- left member symbols
    s1s = UT.toList u1 -- right member symbols

    in2s :: [a] -> [(a,a)]
    in2s (a:b:rest) = (a,b):in2s rest
    in2s _ = []

-- WHERE --

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the joint in the type
mutsOf :: (Sym,Sym) -> SymState -> SymState -> [Mutation]
mutsOf (s0,s1) sst0 sst1 = case (mem0, mem1) of
    (False, True) -> [AddLeft s0]
    (True, False) -> [AddRight s1]
    (False, False) | nic0 == 0 && nic1 == 0 -> [Add2 s0 s1]
                   | otherwise -> [] -- some other mut intros s0 or s1
    (True, True)
      | d0s == IS.singleton s1
      , d1s == IS.singleton s0 -> [Del2 s0 s1]
      | otherwise -> (if IS.null d0s then (DelLeft s0:) else id) $
                     (if IS.null d1s then (DelRight s1:) else id) []
  where
    SymState mem0 (nic0 :!: _) d0s _ = sst0
    SymState mem1 (nic1 :!: _) d1s _ = sst1

-- | Compute the difference in delta-info of a mutation, given all
-- required params and the difference in params it would result in
evalMutation :: Int -> Int -> U.Vector Count -> JointType -> Int ->
                IntMap Count -> Mutation -> MutEntry -> Double
evalMutation m bigN ns jt nm dns mut (MutEntry ddnm ddns _) = loss
  where
    loss = mutLoss m bigN ils' nm' vm'
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

----------
-- MATH --
----------

-- | Loss function is the delta-delta-info of mutations without the
-- terms which are constant accross all mutations
mutLoss :: Int -> Int -> [(Int,Int)] -> Int -> Int -> Double
mutLoss m bigN ils' nm' vm' = nLoss + sLoss + rLoss
  where
    nLoss = logFact (m + bigN - nm')
    sLoss = logFact nm' + sum (uncurry (-) . both logFact <$> ils')
    rLoss = fromIntegral nm' * ilog vm'

-- | Return the difference between the deltaDeltaInfo and the
-- mutLoss. This is the sum of the terms of deltaDeltaInfo which are
-- constant across all mutations.
mutLossComplement :: Int -> Int -> Int -> Int -> Double
mutLossComplement m bigN nm vm = nLossC + sLossC + rLossC
  where
    nLossC = - logFact (m + bigN - nm)
    sLossC = logFact nm
    rLossC = - (fromIntegral nm * ilog vm)

-- | Compute the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDeltaInfo :: Int -> Int -> [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Double
deltaDeltaInfo m bigN ils' (nm,nm') (vm,vm') = nDeltaDelta
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
deltaInfo :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Double
deltaInfo m bigN nm ils vm = nDelta + sDelta + rDelta
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
