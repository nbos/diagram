{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Diagram.Sites (module Diagram.Sites) where

import Control.Monad
import Control.Lens hiding (Index)
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.ST

import Data.Function
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor
import qualified Data.List.Extra as L

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Set as Set

import Data.Strict.Tuple (Pair((:!:)),(:!:))

import qualified Data.Vector.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)

import Streaming hiding (first,second)
import qualified Streaming.Prelude as S

import Diagram.UnionType (Sym)
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D

import Diagram.Util

data Sites = Sites
  { _runHeads       :: !(IntMap ())
  , _bySndConstr    :: !(IntMap Index)
  , _byFstNonConstr :: !(IntMap ())
  , _runTails       :: !(IntMap ()) }
  deriving(Show,Eq)
makeLenses ''Sites

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
--   for a head to---

join :: Sites -> Sites -> Sites
join (Sites hds0 bySnd0 ncs0 tls0) (Sites hds1 bySnd1 ncs1 tls1) = undefined
  where
    safe0s = hds0 IM.\\ bySnd1
    broken0s = hds0 `IM.intersection` bySnd1

    safe1s = hds1 IM.\\ bySnd0
    broken1s = hds1 `IM.intersection` bySnd0

    hds = IM.unionWithKey err safe0s safe1s
    
    -- aConfirm = (a^.byFstConstr) IM.\\ (b^.bySndConstr)
    -- bConfirm = (b^.byFstConstr) IM.\\ (a^.bySndConstr)

    -- abConfirm = IM.unionWithKey err aConfirm bConfirm

    -- aConflict = (a^.byFstConstr) `IM.intersection` (b^.bySndConstr)
    -- bConflict = (b^.byFstConstr) `IM.intersection` (a^.bySndConstr)

    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("Sites.join: collision: " ++) . show .:. (,,)
