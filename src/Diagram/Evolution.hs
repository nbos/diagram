{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Diagram.Evolution (module Diagram.Evolution) where

import System.Random (StdGen)

import Control.Monad.Extra
import Control.Monad.State.Strict
import Control.Monad.Trans.Random.Lazy (RandT)
import Control.Lens hiding (both,last1,Index,(:>),index)

import Diagram.String
import Diagram.Primitive
import Diagram.Streaming ()
import qualified Data.Vector.Unboxed as U

import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import qualified Diagram.JointType.Random as JT

import Diagram.Joints (Joints)
import qualified Diagram.Joints as Jts
import Diagram.ConstrIntervals.Internal (CIs)
import qualified Diagram.ConstrIntervals.Internal as CIs

import Diagram.Evolution.Run ()
import qualified Diagram.Evolution.Run as Run

data EvolutionState

-- | Keep generating random JointTypes and hill-climbing to minima,
-- printing results.
searchForever :: forall m. (MonadIO m, PrimMonad m) => Int -> Int ->
  Doubly (PrimState m) -> U.Vector Int -> Joints CIs -> RandT StdGen m ()
searchForever m bigN dly ns allCIs = go 1
  where
    top = JT.fromJoints allCIs
    allCIs2s = Jts.sized $ Jts.doubleIndex 256 allCIs

    go :: Int -> RandT StdGen m ()
    go !num = do
      (jt, cis) <- JT.genRandom allCIs2s
      -- report stats, verify properties/integrity
      let indent = (liftIO (putStr "  ") >>)
      indent $ JT.printInfo (top, allCIs) (jt, cis)
      liftIO $ putStrLn ""
      indent $ JT.printLUB jt cis
      indent $ JT.printSubtyping (top, allCIs) (jt, cis)
      indent $ JT.printConservation (top, allCIs) (jt, cis)
      indent $ JT.printMembership allCIs (jt, cis)
      liftIO $ putStrLn ""

      (JT u0' u1') <- hillClimb m bigN dly ns allCIs num (jt, cis)
      liftIO $ putStr "Minimal type achieved: " >> print (UT.toAscList u0')
      liftIO $ putStr "                       " >> print (UT.toAscList u1')

      go (num+1) -- repeat

-- | Hill climb to a local minimum, given parameters m (number of
-- symbols), N (string length), the string, symbol counts, constructive
-- intervals of all joints in the string and a joint type with its CIs
-- indexed by joint.
hillClimb :: PrimMonad m => Int -> Int -> Doubly (PrimState m) ->
  U.Vector Int -> Joints CIs -> Int -> (JointType, Joints CIs) -> m JointType
hillClimb m bigN dly ns allCIs num jt = do
  st <- Run.init_ m bigN dly ns allCIs num jt
  res <- execStateT (whileM Run.step) st
  return (res^.Run.typeCIs.CIs.jointType)
