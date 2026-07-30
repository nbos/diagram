{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Evolution.SymEntry (module Diagram.Evolution.SymEntry) where

import Control.Lens hiding (both,last1,Index,(:>))

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Diagram.String
import Diagram.Evolution.Mutation (Mutation(..))


data SymEntry = SymEntry
  { _isMember   :: !Bool -- ^ True iff self is member of the union type
  , _coSymsIn   :: !IntSet -- ^ Symbols that have a joint with
                           -- self and member of the co-union
  , _dependents :: !IntSet -- ^ CoSymsIn that have self as only coSymsIn
  , _coSymsOut  :: !IntSet } -- ^ Symbols that have a joint with self and
                             -- *not* member of the co-union
  deriving (Show,Eq,Ord)
makeLenses ''SymEntry

emptyIn :: SymEntry
emptyIn = SymEntry True IS.empty IS.empty IS.empty

emptyOut :: SymEntry
emptyOut = SymEntry False IS.empty IS.empty IS.empty

mutsOf :: (Sym, SymEntry) -> (Sym, SymEntry) -> [Mutation]
mutsOf se0@(_, SymEntry mem0 _ _ _) se1@(_, SymEntry mem1 _ _ _)
  | mem0, mem1 = delMutsOf se0 se1
  | Just mut <- addMutOf se0 se1 = [mut]
  | otherwise = []

-- | Give the (possibly missing) mutation that would make the given
-- joint member of the type (assumes it's not member of the type and
-- that it exists in the string, i.e. an out-joint)
addMutOf :: (Sym, SymEntry) -> (Sym, SymEntry) -> Maybe Mutation
addMutOf (s0, SymEntry mem0 ic0s _ _) (s1, SymEntry mem1 ic1s _ _)
  | mem0 = Just $ AddRight s1 -- assert (not mem1)
  | mem1 = Just $ AddLeft s0  -- assert (not mem0)
  | IS.null ic0s && IS.null ic1s = Just $ Add2 s0 s1
  | otherwise = Nothing -- some other mut intros s0 or s1

-- | Give the (possibly empty) set of available Del mutations that would
-- take the given joint out of the type (assumes it's member of the type
-- and present in the string, i.e. an in-joint)
delMutsOf :: (Sym, SymEntry) -> (Sym, SymEntry) -> [Mutation]
delMutsOf (s0, SymEntry _ _ d0s _) (s1, SymEntry _ _ d1s _)
  | d0s == IS.singleton s1
  , d1s == IS.singleton s0 = [Del2 s0 s1] -- co-dependent
  | otherwise = [ DelLeft s0 | IS.null d0s ]
                ++ [ DelRight s1 | IS.null d1s ]
