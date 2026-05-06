{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Diagram.Evolution (module Diagram.Evolution) where

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict (StateT(..))

import Data.Strict (type (:!:), Pair((:!:)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
  , _counts :: !(U.Vector Count) -- (readonly)

  -- joint type
  , _jointType :: !JointType
  , _leftSyms  :: !(MV.MVector s SymState) -- :: [m]SymState
  , _rightSyms :: !(MV.MVector s SymState) -- :: [m]SymState

  , _constructed :: !(BV.MVector s Bit) -- :: [N]Bool, only toggle s0s
  , _count :: !Count -- constructed popCount
  , _deltaCounts :: !(IntMap Int) -- dns :: u0 U u1 -> dn

  -- mutations
  , _entries :: !(Map Mutation MutEntry) -- mut -> ddns
  , _byLoss :: !(Map Double Mutation) } -- delta loss -> mut

-- SYMBOL MEMBERSHIP INFO --

data SymState = SymState
  { _member :: !Bool -- ^ True iff self is member of the union type
  , _memCoSyms :: !(Count :!: IntSet) -- ^ Other-side member symbols
                                      -- that have a joint with self
  , _dependents :: !IntSet } -- ^ CoSyms that have self as only CoSym
                             -- (not null only if member)
  deriving (Show,Eq,Ord)

emptyIn :: SymState
emptyIn = SymState True (0 :!: IS.empty) IS.empty

emptyOut :: SymState
emptyOut = SymState False (0 :!: IS.empty) IS.empty

-- MUTATION INFO --

data MutEntry = MutEntry
  { _ddCount :: !Int           -- delta delta nm
  , _ddCounts :: !(IntMap Int) -- delta delta string ns
  , _ddSites :: !Sites }       -- constr. sites
  deriving (Show,Eq)

makeLenses ''SymState
makeLenses ''MutEntry
makeLenses ''EvolutionState

-------------
-- GETTERS --
-------------

-- | m
numSymbols :: Monad m => EvolutionT m Int
numSymbols = leftSyms `uses` MV.length

-- | N, bigN
stringLen :: Monad m => EvolutionT m Int
stringLen = constructed `uses` MU.length

-- | Retrieve the mutation with the minimal delta loss.
getMin :: Monad m => EvolutionT m (Double, Mutation)
getMin = uses byLoss M.findMin

-- | Compute the loss in information/code length incurred by the
-- introduction of the current joint type (no further mutation)
getCurrentLoss :: Monad m => EvolutionT m Double
getCurrentLoss = do
  m <- numSymbols
  bigN <- stringLen
  nm <- use count

  ns <- use counts
  dns <- uses deltaCounts $ IM.elems . IM.mapWithKey (\s adn ->
    let ni = ns U.! s
        ni' = ni - adn
    in (ni,ni')) -- (before, after)

  vm <- uses jointType JT.variety
  return $ deltaInfo m bigN nm dns vm

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

  -- cosyms
  forM_ (M.keys members) $ \(s0,s1) -> do
    MV.modify uLeft (memCoSyms %~ ((+1) `bimap` IS.insert s1)) s0
    MV.modify uRight (memCoSyms %~ ((+1) `bimap` IS.insert s0)) s1

  -- deps
  forM_ s0s $ \s0 -> do
    SymState _true (nc0 :!: mcs0) _ <- MV.read uLeft s0
    when (nc0 == 1) $ case IS.toList mcs0 of
      [s1] -> MV.modify uRight (dependents %~ IS.insert s0) s1
      _else  -> error "expected singleton"
  forM_ s1s $ \s1 -> do
    SymState _true (nc1 :!: mcs1) _ <- MV.read uRight s1
    when (nc1 == 1) $ case IS.toList mcs1 of
      [s0] -> MV.modify uLeft (dependents %~ IS.insert s1) s0
      _else  -> error "expected singleton"
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
        let jtddns = snd $ Sites.join memSites sites -- joint ddns
            ddnm = sum jtddns `div` 2
            sddns = negate <$> jtddns -- string ddns := - joint ddns
        in MutEntry ddnm sddns sites

      Del -> do
        let (Sites ddns h2ts _) = sites
        ddns' <- flip2 foldM ddns (IM.toList h2ts) $
          \im (hd, len :!: (_, s1)) -> do
            hdIsConstr <- unBit <$> MU.read constr hd
            if hdIsConstr then return im else do
              s0 <- D.read str hd
              return $ decr s0 $ if odd len then im else decr s1 im
        let ddnm = sum ddns' `div` 2
        return $ MutEntry ddnm ddns' sites

  let mutsByLoss = M.fromList $ (<$> M.toList mutEntries) $
                   \(mut,me) -> (deltaLoss mut me, mut)
  return $
    EvolutionState str ns jt uLeft uRight constr nm dns mutEntries mutsByLoss

  where
    memSites@(Sites dns runs _) = foldr1 (<>) members
    nm = sum dns `div` 2
    vm = JT.variety jt

    deltaLoss = deltaLossOf m bigN ns nm dns vm

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
mutsOf (s0,s1) st0 st1 = case (mem0, mem1) of
    (False, True) -> [AddLeft s0]
    (True, False) -> [AddRight s1]
    (False, False) | nc0 == 0 && nc1 == 0 -> [Add2 s0 s1]
                   | otherwise -> [] -- some other mut intros s0 or s1
    (True, True)
      | ds0 == IS.singleton s1
      , ds1 == IS.singleton s0 -> [Add2 s0 s1]
      | otherwise -> (if IS.null ds0 then (DelLeft s0:) else id) $
                     (if IS.null ds1 then (DelRight s1:) else id) []
  where
    SymState mem0 (nc0 :!: _) ds0 = st0
    SymState mem1 (nc1 :!: _) ds1 = st1

-- | Compute the difference in loss of a mutation, given all requires
-- params and the difference in params it would produce (MutEntry)
deltaLossOf :: Int -> Int -> U.Vector Count -> Int -> IntMap Count -> Int ->
               Mutation -> MutEntry -> Double
deltaLossOf m bigN ns nm dns vm mut (MutEntry ddnm ddns _) = dl
  where
    dl = deltaDeltaInfo m bigN (nm,nm') (IM.elems dns') (vm,vm')
    nm' = nm + ddnm
    dns' = flip2 IM.intersectionWithKey dns ddns $ \s dn ddn ->
      let n = ns U.! s
          n' = n + dn
          n'' = n' + ddn
      in (n',n'')
    vm' = (vm +) $ case mut of
      AddLeft _  ->  1
      AddRight _ ->  1
      Add2 _ _   ->  2
      DelLeft _  -> -1
      DelRight _ -> -1
      Del2 _ _   -> -2

-- | Compute the info delta from the introduction of a joint type given
-- parameters
deltaInfo :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Double
deltaInfo m bigN nm dns vm = nDelta + sDelta + rDelta
  where
    nDelta = logFact (mpN - nm) - ilog m - logFact (mpN - 1)
    mpN = m + bigN

    sDelta = sDltm - logFact nm
    sDltm = sum $ (<$> dns) $ \(ni,ni') -> logFact ni - logFact ni'

    rDelta = fromIntegral nm * ilog vm

-- | Compute the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDeltaInfo :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> Double
deltaDeltaInfo m bigN (nm,nm') dns (vm,vm') =
  nDeltaDelta + sDeltaDelta + rDeltaDelta
  where
    nDeltaDelta = logFactNpmmnm' - logFactNpmmnm
    logFactNpmmnm = logFact $ mpN - nm
    logFactNpmmnm' = logFact $ mpN - nm'
    mpN = m + bigN -- m + N

    sDeltaDelta = sDDltm + logFact nm - logFact nm'
    sDDltm = sum $ (<$> dns) $ \(ni',ni'') ->
      logFact ni' - logFact ni''
      -- (`logFact ni` (old symbol count) cancel out)

    rDeltaDelta = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral
