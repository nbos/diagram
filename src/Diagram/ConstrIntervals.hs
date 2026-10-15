{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Diagram.ConstrIntervals ( module Diagram.ConstrIntervals
                               , module Diagram.ConstrIntervals.Internal ) where

import Debug.Trace
import GHC.Utils.Monad

import Control.Monad hiding (join)
import Control.Lens hiding (Index,(:>))
import Control.Monad.State.Strict

import Data.Maybe
import Data.Bifunctor
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive

import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import qualified Diagram.UnionType as UT
import Diagram.ConstrInterval (CI(..))
import qualified Diagram.ConstrInterval as CI

import Diagram.Util

import Diagram.ConstrIntervals.Internal

-- This module only contains the join and difference operations, with
-- their state structs and lenses. CIs struct, construction and debug
-- functions are in Diagram.ConstrIntervals.Internal.

----------
-- JOIN --
----------

data JoinState = JoinState
  { __A :: !CIs -- some set of intervals 'A'
  , __B :: !CIs -- some other set of intervals 'B'
  , _joinCor :: !(IntMap Int) -- sym count delta accumulator
} deriving (Show,Eq)
makeLenses ''JoinState

instance Semigroup CIs where
  (<>) :: CIs -> CIs -> CIs
  (<>) = join

instance Monoid CIs where
  mempty :: CIs
  mempty = empty

-- | "Union" or "join" of two sets of constructive intervals which are
-- assumed to be disjoint in their joint types (implying also disjoint
-- in the constructive sites/intervals). Under this assumption,
-- collisions only have to be detected on the ends (head and tail) of
-- each interval without considering the symbols in between.
join :: CIs -> CIs -> CIs
join = fst .: join_

-- | More general join function which leaks the difference in counts
-- between the sum of the counts of each set of intervals and the counts
-- of the returned set of intervals (snd)
join_ :: CIs -> CIs -> (CIs, IntMap Int)
join_ ciAs ciBs = flip evalState (JoinState ciAs ciBs IM.empty) $ do
  traceM "Called join_"
  checkIntegrity_ ciAs
  traceM $ "ciAs (valid) = " ++ pShow ciAs
  checkIntegrity_ ciBs
  traceM $ "ciBs (valid) = " ++ pShow ciBs

  -- short-circuit intersection (O(m) <<< O(N)) --
  unless (uB0 `UT.disjoint` uA1) $
    uses2 (_B.byHead) (_A.byTail) IM.intersection
    >>= mapM_ go
  --

  -- short-circuit intersection (O(m) <<< O(N)) --
  unless (uA0 `UT.disjoint` uB1) $ do
    cols <- uses2 (_A.byHead) (_B.byTail) IM.intersection
    modify $ \(JoinState a b d) -> JoinState b a d -- A <--> B
    mapM_ go cols
    modify $ \(JoinState b a d) -> JoinState a b d -- B <--> A
  --

  -- fold new sym count deltas into the counts map
  cor <- use joinCor
  bhd <- uses2 (_A.byHead) (_B.byHead) $ IM.unionWithKey err'
  btl <- uses2 (_A.byTail) (_B.byTail) $ IM.unionWithKey err'
  let ns' = -- L.foldl' (flip $ uc alter) ns (IM.toList dns)
        IM.mergeWithKey (\_ n dn -> nothingIf (==0) (n + dn))
        id id ns cor
      res = CIs jt ns' bhd btl

  traceM $ "res = " ++ pShow res
  return ( checkIntegrityId res -- (debug)
         , cor )

  where
    JT uA0 uA1 = ciAs^.jointType
    JT uB0 uB1 = ciBs^.jointType
    jt = JT.join (ciAs^.jointType) (ciBs^.jointType)
    ns = IM.unionWith (+) (ciAs^.symCounts) (ciBs^.symCounts)

    alter s d = flip IM.alter s $ \case
      Nothing -> Just d
      Just n -> nothingIf (== 0) (n + d)

    -- inc = inc_ 1
    dec = inc_ (-1)
    inc_ :: Int -> Sym -> StateT JoinState Identity ()
    inc_ d s = joinCor %= alter s d

    -- | Given a constructive interval from 'B' whose head collides with
    -- the tail of an interval of the other set 'A', join together, fix
    -- count if required and re-insert in 'A' set
    go :: CI -> StateT JoinState Identity ()
    go ciB@(CI tlA@hdB _ lenB tlB stlB) = do
      -- can't accept a ciA independent of any previous go calls (as in
      -- IM.intersectionWith (,)) because it could have been pre-pended
      -- to (sandwich case), so we lookup (NOTE: doesn't that only
      -- happen on the second pass though?)
      ciA@(CI hdA _ lenA _ stlA) <-
        _A.byTail %%= first fromJust . deleteLookup tlA -- delete [.. tlA]

      _B.byHead %= IM.delete hdB -- delete [hdB ..]
      _B.byTail %= IM.delete tlB -- delete [.. tlB]

      when (even lenA) $ dec stlA
      let ciAB@(CI _ _ lenAB _ _) = CI.unsafeJoin ciA ciB

      ((_A.byHead) %%= deleteLookup tlB >>=) $ \case
        -- simple collision: [hdA..tlA) <> [hdB..tlB] ==> [hdA..tlB]
        Nothing -> do
          _A.byHead %= IM.insert hdA ciAB -- update hdA
          _A.byTail %= IM.insert tlB ciAB -- insert tlB
          let d = fromEnum (even lenAB) - fromEnum (even lenB)
          unless (d == 0) $ inc_ d stlB

        -- sandwich: [hdA..tlA) <> [hdB..tlB) <> [hdA2..tlA2] ==> [hdA..tlA2]
        Just ciA2@(CI hdA2 _ lenA2 tlA2 stlA2) -> do
          when (even lenB) $ dec stlB
          let ciABA@(CI _ _ lenABA _ _) = CI.unsafeJoin ciAB ciA2
          _A.byHead %= IM.delete hdA2 -- delete hdA2
          _A.byHead %= IM.insert hdA ciABA -- update hdA
          _A.byTail %= IM.insert tlA2 ciABA -- update tlA2
          let d = fromEnum (even lenABA) - fromEnum (even lenA2)
          unless (d == 0) $ inc_ d stlA2

    err' :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err' = error . ("join: collision: " ++) . show .:. (,,)

deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)
{-# INLINE deleteLookup #-}

debug_join :: PrimMonad m => Doubly (PrimState m) -> CIs -> CIs -> m CIs
debug_join = fmap fst .:. debug_join_

debug_join_ :: PrimMonad m => Doubly (PrimState m) ->
               CIs -> CIs -> m (CIs, IntMap Int)
debug_join_ dly cisA cisB = do
  unlessM (valid dly cisA) $ do
    traceM' $ "supplied CIs not valid (left): \n" ++ show cisA
    checkIntegrityM dly cisA
  unlessM (valid dly cisB) $ do
    traceM' $ "supplied CIs not valid (right): \n" ++ show cisB
    checkIntegrityM dly cisB
  unlessM (valid dly cisC) $ do
    traceM' $ "join CIs not valid. \n\n"
      ++ "left: " ++ show cisA ++ "\n\n"
      ++ "right: " ++ show cisB ++ "\n\n"
      ++ "join: " ++ show cisC ++ "\n"
    checkIntegrityM dly cisC
  -- traceM' $ "CIs join OK: " ++ pShow (toList cisC)
  return res
  where
    res@(cisC,_) = join_ cisA cisB
    traceM' = traceM . ("ConstrIntervals.debug_join_: " ++)

----------------
-- DIFFERENCE --
----------------

data DiffState = DiffState
  { _byHeadRes :: IntMap CI
  , _byTailRes :: IntMap CI
  , _diffCor :: !(IntMap Int) -- sym count delta accumulator
} deriving (Show,Eq)
makeLenses ''DiffState

-- | Remove the constructions of the second set of CIs from the first,
-- given a reference string and optionally membership functions for the
-- super (left) and sub (right) operands. Cannot determine if the joints
-- of the sub set are sufficient to eliminate any symbol of the joint
-- type of the super type without a complete scan of the super's joints,
-- so the type field is left untouched. Also returns (snd) the
-- correction on the counts between the difference in the counts and the
-- counts of the difference (i.e. the Cor of this mut).
difference :: PrimMonad m => Doubly (PrimState m) ->
  Maybe (Sym -> Sym -> m Bool) -> Maybe (Sym -> Sym -> m Bool) ->
  CIs -> CIs -> m (CIs, IntMap Int)
difference dly memSuper_ memSub_ cisA cisB@(CIs jtB _ bhdB _) = do
  checkIntegrity_ cisA -- (debug)
  checkIntegrity_ cisB -- (debug)
  traceM $ " CIs.difference called, args  = " ++ pShow (cisA, cisB)

  DiffState bhdA' btlA' dnsA <- flip execStateT (DiffState bhdA btlA IM.empty) $
    forM_ (IM.elems bhdB) $ \ci@(CI subHd _ _ subTl _) -> (getSuper ci >>=) $ \case
      Nothing -> do -- super doesn't start here, but ci is inside it
        ((diffCor %=) . imUnion) =<< CI.symCounts dly ci
      Just Nothing -> do -- super is identical, remove indexes, cor is 0
        byHeadRes %= IM.delete subHd
        byTailRes %= IM.delete subTl
      Just (Just (super@(CI superHd _ _ superTl _), rems)) -> do -- case super
        byHeadRes %= IM.delete superHd
        byTailRes %= IM.delete superTl
        ciCounts <- CI.symCounts dly ci
        supCounts <- CI.symCounts dly super
        diffCor %= imUnion ((negate <$> ciCounts) `imUnion` supCounts)
        forM_ rems $ \r@(CI remHd _ _ remTl _) -> do
          byHeadRes %= IM.insert remHd r
          byTailRes %= IM.insert remTl r
          remCounts <- CI.symCounts dly r
          diffCor %= imUnion (negate <$> remCounts)

  let nsA' = imUnion nsA dnsA
      res = CIs jtA nsA' bhdA' btlA'
  traceM $ " res = " ++ pShow res
  checkIntegrity_ res -- (debug)
  return (res, dnsA)

  where
    CIs jtA nsA bhdA btlA = cisA
    memSuper = fromMaybe (return .: JT.member jtA) memSuper_
    memSub   = fromMaybe (return .: JT.member jtB) memSub_
    getSuper = lift . CI.superCI dly memSuper memSub
