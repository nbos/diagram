{-# LANGUAGE TemplateHaskell #-}
module Diagram.Evolution.StringState (module Diagram.Evolution.StringState) where

import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import Data.IntMap.Strict (IntMap)

import Data.Bit (Bit(..))
import qualified Data.Bit as BV
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed as U

import Diagram.String
import Diagram.Primitive

type StringT m = StateT (StringState (PrimState m)) m
data StringState s = STR
  { _doubly :: !(Doubly s) -- underlying string :: [N]Sym (readonly)
  , _symCounts :: !(U.Vector Count) -- symbol counts (readonly) TODO: dyn?
  , _constructed :: !(BV.MVector s Bit) -- :: [N]Bool, only s0s toggled
  , _symDeltas :: !(IntMap Int) -- delta symbol count :: u0 U u1 -> dn
  , _jointCount :: !Count } -- joint count, popCount of constructed
makeLenses ''StringState

-- | N, bigN
stringLen :: Monad m => StringT m Int
stringLen = constructed `uses` MU.length
