{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module Diagram.Evolution (module Diagram.Evolution) where

import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict ( StateT(..)
                                  , MonadState(get) )

import Data.Strict hiding (isLeft, isRight)
import Data.Map.Strict (Map)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Data.Bit (Bit(..))
import qualified Data.Bit as BV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U

import Diagram.Primitive
import Diagram.Information

import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import Diagram.String

data Mutation = AddLeft  !Sym
              | AddRight !Sym
              | Add2     !Sym !Sym
              | DelLeft  !Sym
              | DelRight !Sym
              | Del2     !Sym !Sym
  deriving(Show,Eq,Ord)

data SymState = SymState
  { _member :: !Bool -- true if self is member of the union type, false otherwise
  , _memCoSyms :: !(Count :!: IntSet) -- other side member syms w/ a joint w/ self
  , _dependents :: !IntSet } -- cosyms that have self as only cosym
  deriving (Show,Eq,Ord)
makeLenses ''SymState

emptyIn :: SymState
emptyIn = SymState True (0 :!: IS.empty) IS.empty

emptyOut :: SymState
emptyOut = SymState False (0 :!: IS.empty) IS.empty

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
data EvolutionState s = EvolutionState
  -- string O(N)
  { _string :: !(Doubly s) -- underlying string :: [N]Sym
  , _counts :: !(U.Vector Int)
  , _constructed :: !(BV.MVector s Bit) -- :: [N]Bool
  , _count :: !Count -- constructed popCount

  -- joint type O(mc)
  , _jointType :: !JointType
  , _leftSyms  :: !(MV.MVector s SymState) -- :: [m]SymState
  , _rightSyms :: !(MV.MVector s SymState) -- :: [m]SymState
  , _deltaCounts :: !(IntMap Count) -- :: u0 U u1 -> dn

  -- mutations O(m)
  , _deltas :: !(Map Mutation (IntMap Count :!: IntSet)) -- mut -> (dns :!: is)
  , _mutations :: !(Map Double Mutation) } -- delta loss -> mut
makeLenses ''EvolutionState

-- | m
numSymbols :: Monad m => EvolutionT m Int
numSymbols = leftSyms `uses` MV.length

-- | N, bigN
stringLen :: Monad m => EvolutionT m Int
stringLen = constructed `uses` MU.length

-- | Compute the loss in information/code length incurred by the
-- introduction of the current joint type (no additional mutation)
getLoss :: Monad m => EvolutionT m Double
getLoss = do
  m <- numSymbols
  bigN <- stringLen
  nm <- use count

  ns <- use counts
  dns <- uses deltaCounts $ IM.elems . IM.mapWithKey (\s adn ->
    let ni = ns U.! s
        ni' = ni - adn
    in (ni,ni')) -- (before, after)

  vm <- uses jointType JT.variety
  return $ delta m bigN nm dns vm

-- | Compute the info delta from the introduction of a joint type given
-- parameters
delta :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Double
delta m bigN nm dns vm = nDelta + sDelta + rDelta
  where
    nDelta = logFact (mpN - nm) - ilog m - logFact (mpN - 1)
    mpN = m + bigN

    sDelta = sDltm - logFact nm
    sDltm = sum $ (<$> dns) $ \(ni,ni') -> logFact ni - logFact ni'

    rDelta = fromIntegral nm * ilog vm

-- | Compute the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDelta :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> Double
deltaDelta m bigN (nm,nm') dns (vm,vm') =
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

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the joint in the type
mutOf :: PrimMonad m => (Sym,Sym) -> EvolutionT m [Mutation]
mutOf (s0,s1) = do
  (EvolutionState _ _ _ _ _ v0 v1 _ _ _) <- get
  SymState mem0 (nc0 :!: _) ds0 <- MV.read v0 s0
  SymState mem1 (nc1 :!: _) ds1 <- MV.read v1 s1

  return $ case (mem0, mem1) of
    (False, True) -> [AddLeft s0]
    (True, False) -> [AddRight s1]
    (False, False) | nc0 == 0 && nc1 == 0 -> [Add2 s0 s1]
                   | otherwise -> [] -- some other mut intros s0 or s1
    (True, True)
      | ds0 == IS.singleton s1
      , ds1 == IS.singleton s0 -> [Add2 s0 s1]
      | otherwise -> (if IS.null ds0 then (DelLeft s0:) else id) $
                     (if IS.null ds1 then (DelRight s1:) else id) []
