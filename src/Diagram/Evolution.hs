{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Diagram.Evolution (module Diagram.Evolution) where

import System.Random (StdGen)
import Text.Printf

import Control.Monad.State.Strict
import Control.Monad.Trans.Random.Lazy (RandT)
import Control.Lens hiding (both,last1,Index,(:>),index)

import Data.Hashable

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive
import Diagram.Streaming ()
import qualified Data.Vector.Unboxed as U

import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType.Random as JT

import Diagram.Joints (Joints)
import qualified Diagram.Joints as Jts
import Diagram.ConstrIntervals.Internal (CIs)
import qualified Diagram.ConstrIntervals.Internal as CIs

import Diagram.Evolution.MutEntry (MutEntry(ME))

import Diagram.Evolution.Run (RunState)
import qualified Diagram.Evolution.Run as Run
import Diagram.Util

data EvolutionState = EvolutionState

-- | Keep generating random JointTypes and hill-climbing to minima,
-- printing results.
searchForever :: forall m. (MonadIO m, PrimMonad m) => Int -> Int ->
  Doubly (PrimState m) -> U.Vector Int -> Joints CIs -> RandT StdGen m ()
searchForever m bigN dly ns allCIs = go 1
  where
    allCIs2s = Jts.sized $ Jts.doubleIndex m allCIs

    go :: Int -> RandT StdGen m ()
    go !num = do
      (jt@(JT u0 u1), cis) <- JT.genRandom allCIs2s
      liftIO $ putStrLn $ "Generated random type [" ++ shortHashStr jt ++ "]:"
      liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0
      liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1 ++ " )"

      jt'@(JT u0' u1') <- hillClimb m bigN dly ns allCIs (jt, cis)
      liftIO $ putStrLn $ "Minimal type achieved [" ++ shortHashStr jt' ++ "]:"
      liftIO $ putStrLn $ "( 0x" ++ UT.toHex m u0'
      liftIO $ putStrLn $ ", 0x" ++ UT.toHex m u1' ++ " )"
      liftIO $ putStrLn ""
      go (num+1) -- repeat

-- | Hill climb to a local minimum, given parameters m (number of
-- symbols), N (string length), the string, symbol counts, constructive
-- intervals of all joints in the string and a joint type with its CIs
-- indexed by joint.
hillClimb :: forall m. (PrimMonad m, MonadIO m) => Int -> Int ->
             Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
             (JointType, Joints CIs) -> m JointType
hillClimb = Run.init_ >======> go
  where
    go :: RunState (PrimState m) -> m JointType
    go st = do
      let jt = Run.jointType st
          h = shortHashStr jt
      (ddInfo, me@(ME mut _ _ _ _)) <- Run.bestMut st
      if ddInfo <= 0
        then do
        liftIO $ putStr   $ "" ++ gray ++ h ++ reset ++ " "
        liftIO $ putStr   $ printf "%-22s" (pShowMut mut)
        liftIO $ putStrLn $ printf "%-11.4e" ddInfo
        execStateT (Run.pushMut me) st >>= go

        else
        return $ st^.Run.typeCIs.CIs.jointType

hashStr :: Hashable a => a -> String
hashStr = printf ("%016x") . hash

shortHashStr :: Hashable a => a -> String
shortHashStr = take 7 . printf ("%016x") . hash
