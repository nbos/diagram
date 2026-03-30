{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
module Diagram.Sites (module Diagram.Sites) where

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (Index)
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.State.Strict

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Strict.Tuple (Pair((:!:)),(:!:))

import Diagram.UnionType (Sym)
import Diagram.Joints (Doubly)
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D

import Diagram.Util

data Sites = Sites
  { _jointType      :: !JointType
  , _runHeads       :: !IntSet
  , _bySndConstr    :: !IntSet }
  deriving(Show,Eq)
makeLenses ''Sites

join :: forall m. PrimMonad m => Doubly (PrimState m) -> Sites -> Sites -> m Sites
join ss sitesA sitesB = do
  sitesA' :!: sitesB' <- flip execStateT (sitesA :!: sitesB) $ do
    forM_ (IS.elems brokenAs) $ assertNext >=> goBroken _2 _1
    forM_ (IS.elems brokenBs) $ assertNext >=> goBroken _1 _2

  undefined

  where
    -- Given lenses into the breaking and broken sides, treat the
    -- breaking of a joint at (i0,i1) from a joint at (im1,i0), given
    -- only index i1.
    goBroken :: Lens' (Sites :!: Sites) Sites -> Lens' (Sites :!: Sites) Sites ->
                Index -> StateT (Sites :!: Sites) m ()
    goBroken breaking broken = go
      where
        go :: Index -> StateT (Sites :!: Sites) m ()
        go i1 = do
          broken.bySndConstr %= IS.delete i1
          whenJustM (next i1) $ \i2 -> do
            s1 <- read i1
            s2 <- read i2
            moreBreaking <- (breaking.jointType) `uses` JT.member (s1,s2)
            if not moreBreaking then zoom broken $ goNonConstr (i1,s1) (i2,s2)
              else whenJustM (next i2) $ \i3 ->
              do s3 <- read i3
                 moreBroken <- (broken.jointType) `uses` JT.member (s2,s3)
                 when moreBroken $ go i3 -- cont.

    -- Upon the breaking of a joint at (i0,i1), at the end of a breaking
    -- run (i.e. no joint at (i1,i2) on the breaking side) repeatedly
    -- check if the freed i1 allows for a previously non-constructive
    -- joint at (i1,i2) to become constructive on the broken side,
    -- taking precedence from a possibly empty tail on the broken side.
    goNonConstr :: (Index,Sym) -> (Index,Sym) -> StateT Sites m ()
    goNonConstr (i1,s1) (i2,s2) = do
      nonConstr <- jointType `uses` JT.member (s1,s2)

      undefined

    Sites jtA hdsA bySndA = sitesA
    Sites jtB hdsB bySndB = sitesB

    safeHdsA = hdsA IS.\\ bySndB
    brokenAs = hdsA `IS.intersection` bySndB

    safeHdsB = hdsB IS.\\ bySndA
    brokenBs = hdsB `IS.intersection` bySndA

    read :: MonadTrans t => Index -> t m Sym
    read = lift . D.read ss
    prev :: MonadTrans t => Index -> t m (Maybe Index)
    prev = lift . D.prev ss
    next :: MonadTrans t => Index -> t m (Maybe Index)
    next = lift . D.next ss
    assertNext :: MonadTrans t => Index -> t m Index
    assertNext = lift . D.unsafeNext ss

    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("Sites.join: collision: " ++) . show .:. (,,)

-- NOTE: For a string of symbols and a joint construction rule which is
--   a function on pairs of symbols (e.g. a decision (s0,s1) -> {True,
--   False}), for each symbol to be part of at most 1 construction,
--   giving the arbitrary precedence to the first joint when a conflict
--   arises, some joints are filtered out (marked non-constructive) on
--   the grounds of their first symbol belongs to the preceeding joint
--   (in the second position).
--
--   When it comes to merging two sets of constructions, the conflicts
--   between joints needs to be re-computed, making some previously
--   constructive joints non-constructive and vice versa.
--
--   We can simplify the computation of conflicts by representing
--   constructive joints of as "runs" of consecutive (non-overlapping)
--   constructions. Because there are no gaps in a run for a joint to
--   squeeze in, if the head of a run is *not* "broken" by a joint
--   appearing right before it, none of the joints in the run will be
--   broken either. If a head *is* broken by a joint, then so are the
--   `n` following joints in that run for a breaking joint with `n` more
--   joints in its run.
--
--   When a head (and a possibly empty set of following joints) is
--   broken by the tail of a new head, it may free a previously
--   unconstructable run, which, by being in off-step with the now
--   broken head of the run, are in sync with the breaking run and
--   appends to it. The run with the broken head only restores a head
--   for itself after both the tail of the breaking run and the
--   previously unconstructable run end, if there are any joints left at
--   that point.
--
--   Some runs in different sets will concatenate without any breaking
--   occuring, simply by being in sync and because one is terminating
--   immediately before the second one begins.
--
--   With this representation, we avoid arbitrarily long chains of
--   lookups to resolve a chain of conflicts because it is impossible
--   for a head that is invalidated/broken--
