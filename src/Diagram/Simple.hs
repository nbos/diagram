{-# LANGUAGE BangPatterns #-}
module Diagram.Simple (module Diagram.Simple) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.String
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))

import Diagram.Util

-- | Return a histogram of the counts of symbols in the given string
symCounts :: [Sym] -> IntMap Count
symCounts = go IM.empty
  where
    go !ns [] = ns
    go !ns (s:ss) = go (inc s ns) ss
    inc s = IM.insertWith (+) s 1

-- | Substitute joints satisfying the given type with a given symbol in
-- the given string
subst :: JointType -> Sym -> [Sym] -> [Sym]
subst (JT u0 u1) s01 = go
  where
    go [] = []
    go [s] = [s]
    go (s0:s1:ss)
      | s0 `UT.member` u0
      , s1 `UT.member` u1 = s01 : go ss
      | otherwise = s0 : go (s1:ss)

-- | Return the symbol counts after the introduction of a given joint
-- type, without the count of the introduced symbol
newSymCounts :: JointType -> [Sym] -> IntMap Count
newSymCounts = IM.delete (-1) . symCounts .: flip subst (-1)
