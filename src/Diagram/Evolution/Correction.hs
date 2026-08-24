{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Diagram.Evolution.Correction (module Diagram.Evolution.Correction) where

import Prelude hiding (init)
import Debug.Trace

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Diagram.Primitive
import Diagram.String
import Diagram.ConstrInterval(CI(..), ciLength, tailIndex)
import qualified Diagram.ConstrInterval as CI

import Diagram.Evolution.Mutation (Mutation(..))
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS

import Diagram.Util

-- Here we compute, for constructive intervals (CIs) and mutations, the
-- difference between the symbol counts of the union/join of the CIs &
-- those of the mutation's (i.e. (cis U mut.cis).ns) and the sum of the
-- symbol counts of the CIs and those of the mutation's (i.e. (cis.ns +
-- mut.cis.ns)).

-- | Given the string, a type, and a constructive interval of the joint
-- type, return the set of corrections on the symCounts of each CIs
-- associated with a mutation (add or del, all at once) required to be
-- added in order for it to match the actual change in symbol counts
-- produced by the mutation. Corrections are signed to be *added* to the
-- CIs.symCounts before they are subtracted (add) or added (del) to the
-- joint type's own CIs.symCounts.
corrsOf :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
corrsOf dly tst ci = fmap clean $ do
  traceShowM ci

  -- [DEL]: decompose, treat all delMuts
  delMutCorrs <- delMutCorrsOf dly tst ci
  traceM $ "del correction: " ++ show delMutCorrs

  -- [ADD]: grab the largest chain possible, if CI is first in the chain
  addMutCorrs <- (prevCI ci >>=) $ \case
    Nothing -> ((traceM "no CI before" >> nextCIs ci) >>=) $ \case
      Nothing -> return M.empty
      Just (addMut, nexts) -> do
        traceM $ "CIs after: " ++ show (addMut,nexts)
        return $ M.singleton addMut $ addMutCorrOf (ci:|nexts)

    Just p@(addMut, prv) -> ((traceM ("prev CI: " ++ show p) >> nextCIs ci) >>=) $ \case
      Nothing -> do
        traceM "no CIs after"
        return $ M.singleton addMut $ addMutCorrOf (prv:|[ci])

      Just p'@(addMut', nexts)
        | addMut == addMut' -> do
            traceM $ "CIs after (same mut): " ++ show p'
            return $ M.singleton addMut $ addMutCorrOf (prv:|ci:nexts)

        | otherwise -> do
            traceM $ "CIs after: " ++ show p'
            return $ M.fromListWithKey col
              [ (addMut, addMutCorrOf (prv:|[ci]))
              , (addMut', addMutCorrOf (ci:|nexts)) ]

  let res = M.unionWithKey col delMutCorrs addMutCorrs
  traceM $ "all correction: " ++ show res
  traceM ""
  return res

  where
    clean = M.filter (not . IM.null) . fmap (IM.filter (/=0))
    err' = err . ("corrsOf: " ++)
    col = err' . ("collision: " ++) . show .:. (,,)

    prevCI = fmap join . TS.prevMutCI dly tst
    nextCIs = TS.nextMutCIs dly tst

-- where --

-- | Given a non-empty list of overlapping (connecting) intervals after
-- an add mutation (alternating [in-]add-in-add-etc.), return the
-- appropriate correction on delta delta symbol counts (ddns)
addMutCorrOf :: NonEmpty CI -> IntMap Int
addMutCorrOf cis = L.foldl' (flip f) IM.empty (NE.init cis) &
  case compare (even newLen) (even oldLen) of
    LT -> IM.insertWith (+) tailSym 1
    EQ -> id
    GT -> IM.insertWith (+) tailSym (-1)
  where
    newLen = sum ((^.ciLength) <$> cis) -- constituents lengths
             - (length cis - 1) -- overlaps

    f (CI _ _ len _ stl) | even len = IM.insertWith (+) stl 1
                         | otherwise = id

    CI _ _ oldLen _ tailSym = NE.last cis

-- | Given a constructive interval of the joint type (in), count all
-- the differences in symbol counts between the symCounts of the CIs
-- for all joints removed by the same del-mutation and and the real
-- difference in symCounts from applying those mutations.
delMutCorrsOf :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
delMutCorrsOf dly tst ci = do

  isConstr <- flip IS.member . IS.fromList . everyOther . fmap fst
              <$> CI.symExtension dly ci

  let go :: Mutation -> Bool -> [CI] -> StateT (Map Mutation (IntMap Int)) m ()
      go delMut = go_ where
        go_ _ [] = return ()
        go_ phase (CI hd shd len tl stl : rest) = do
          unless (tl == (ci^.tailIndex)) $ dec stl -- tl
          let outOfPhase = phase /= isConstr hd
          -- out of phase with super-CI means prev hd will be constr
          -- means hd will still be constr. so hd will not be docked
          when outOfPhase $ dec shd -- hd
          let phase' = phase /= odd len -- xor
          go_ phase' rest

        dec :: Sym -> StateT (Map Mutation (IntMap Int)) m ()
        dec s = modify $ M.insertWith (const $ IM.insertWith (+) s (-1))
                delMut (IM.singleton s (-1))

  flip execStateT M.empty $
    mapM_ (uc $ flip go True) -- True == constr
    . M.toList . M.fromListWith (++)
    . reverse . ffmap (:[]) -- reverse to maintain order
    =<< lift (TS.decomposeIn dly tst ci)

  where
    everyOther :: [a] -> [a]
    everyOther [] = []
    everyOther [a] = [a]
    everyOther (a:_:rest) = a : everyOther rest

err :: [Char] -> a
err = error . ("Correction." ++)
