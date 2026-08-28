{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections #-}

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

import Diagram.Primitive
import Diagram.String
import Diagram.ConstrInterval(CI(..), ciLength)

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
-- CIs.symCounts before they are subtracted/negated (Add) or added (Del)
-- to the joint type's own CIs.symCounts.
corrsOf :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
corrsOf dly tst ci = do
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

    Just p@(addMut, prv) -> ((traceM ("prev CI: " ++ show p)
                              >> nextCIs ci) >>=) $ \case
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

  let res = clean $ M.unionWithKey col delMutCorrs addMutCorrs
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

-- TODO: guards before returning asserting: even (sum res)? (dnm)

-- | Given a non-empty list of overlapping (connecting) intervals after
-- an add mutation (alternating [in-]add-in-add-etc.), return the
-- appropriate correction on the mut's CIs' sym counts.
addMutCorrOf :: NonEmpty CI -> IntMap Int
addMutCorrOf cis = flip execState IM.empty $ do
  forM_ (NE.init cis) $ \(CI _ _ len _ stl) ->
    when (even len) $ modify $ IM.insertWith (+) stl (-1)
  let CI _ _ oldLen _ tailSym = NE.last cis
      newLen = sum (_ciLength <$> cis) -- constituents lengths
               - (length cis - 1) -- overlaps
      d = fromEnum (even newLen) - fromEnum (even oldLen)
  when (d /= 0) $ modify $ IM.insertWith (+) tailSym d

-- | Given a constructive interval of the joint type (in), count all
-- the differences in symbol counts between the symCounts of the CIs
-- for all joints removed by the same del-mutation and and the real
-- difference in symCounts from applying those mutations.
delMutCorrsOf :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
delMutCorrsOf dly tst supCI@(CI _ _ supLen supTl supStl) = do
  fmap go . M.fromListWith (<>)
    . reverse -- preserve order through (<>)
    . ffmap NE.singleton <$> TS.decomposeIn dly tst supCI
  where
    supLenEven = even supLen
    go :: NonEmpty (Bool, CI) -> IntMap Int
    go = flip execState IM.empty . go_ False
      where -- False == aligned with supCI
        go_ :: Bool -> NonEmpty (Bool, CI) -> State (IntMap Int) ()
        go_ prevRemPhase ((hp, CI _ shd len tl stl) :| rest) = do
          let outOfPhase = prevRemPhase /= hp
          -- out of phase with rem means (phd,hd) (which is in phase)
          -- will still be constr after del mut; means hd will still be
          -- constr. so shd's count will not get docked by the mut
          when outOfPhase $ dec shd -- hd
          case rest of
            next:rest' ->
              when lenEven (dec stl)
              >> go_ nextRemPhase (next:|rest')

            _ | tl == supTl ->
                  let d = fromEnum supLenEven - fromEnum lenEven
                  in when (d /= 0) $ inc_ d stl

              | otherwise -> do -- a rem follows
                  when lenEven $ dec stl
                  let supTlSwitchedPhase = nextRemPhase
                  when supTlSwitchedPhase $
                    if supLenEven then inc supStl -- constr. -> non
                    else dec supStl -- non-constr. -> constr.
          where
            lenEven = even len -- means tl is constr.
            nextRemPhase =  -- | even len   = not hp
              lenEven /= hp -- | othwerwise = hp

        -- decrement: every symbol that is counted in the del CI, but will
        -- still be constr. in the remainder CI
        dec :: Sym -> State (IntMap Int) ()
        dec = inc_ (-1)
        -- increment: every symbol that is not counted in the del CI,
        -- but still gets its count reduced in the remainder CI
        inc :: Sym -> State (IntMap Int) ()
        inc = inc_ 1
        inc_ :: Int -> Sym -> State (IntMap Int) ()
        inc_ d s = modify $ IM.insertWith (+) s d

err :: [Char] -> a
err = error . ("Correction." ++)
