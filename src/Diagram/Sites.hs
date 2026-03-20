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
  { _byFstConstr    :: !(IntMap Index)   -- i0 --> i1
  , _bySndConstr    :: !(IntMap Index)   -- i0 <-- i1
  , _byFstNonConstr :: !(IntMap Index) } -- i0 --> i1
  deriving(Show,Eq)
makeLenses ''Sites

checkIntegrity :: Sites -> a -> a
checkIntegrity sts@(Sites byFst bySnd uncon)
  | IM.size byFst /= IM.size bySnd =
      err $ "different sizes (constructive): "
      ++ show (IM.size byFst, IM.size bySnd)
  | IS.fromList (IM.elems byFst) /= IM.keysSet bySnd =
      err $ "right indexes "
      ++ show (IM.keysSet bySnd IS.\\ IS.fromList (IM.elems byFst))
      ++ " missing from elems of byFst" 
  | IS.fromList (IM.elems bySnd) /= IM.keysSet byFst =
      err $ "left indexes "
      ++ show (IM.keysSet byFst IS.\\ IS.fromList (IM.elems bySnd))
      ++ " missing from elems of bySnd" 
  | not (IM.keysSet uncon `IS.isSubsetOf` IM.keysSet bySnd) =
      err $ "index of nonconstructive joints "
      ++ show (IM.keysSet uncon IS.\\ IM.keysSet bySnd)
      ++ " missing from bySnd (constructive) map"
  | otherwise = id -- [PASS]
  where
    err msg = error ("Sites.checkIntegrity: " ++ msg ++ "\n"
                     ++ show sts)

empty :: Sites
empty = Sites e e e
  where e = IM.empty

insertConstr :: (Index,Index) -> Sites -> Sites
insertConstr (i0,i1) (Sites c0s c1s ncs) = Sites c0s' c1s' ncs
  where c0s' = IM.insert i0 i1 c0s
        c1s' = IM.insert i1 i0 c1s

insertNonConstr :: (Index,Index) -> Sites -> Sites
insertNonConstr = over byFstNonConstr . uncurry IM.insert

join :: Sites -> Sites -> Sites
join a b = undefined
  where
    aConfirm = (a^.byFstConstr) IM.\\ (b^.bySndConstr)
    bConfirm = (b^.byFstConstr) IM.\\ (a^.bySndConstr)

    abConfirm = IM.unionWithKey err aConfirm bConfirm

    aConflict = (a^.byFstConstr) `IM.intersection` (b^.bySndConstr)
    bConflict = (b^.byFstConstr) `IM.intersection` (a^.bySndConstr)

    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("Sites.join: collision: " ++) . show .:. (,,)
