module Diagram.UnionType (module Diagram.UnionType) where

import Prelude hiding (length)

import Data.Tuple.Extra
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as UV
import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Model (Sym)
import Diagram.Information (log2)
import Diagram.Util

-- | A sized union of symbols
data UnionType = U {
  length :: !Int, -- size of the set
  set :: !IntSet  -- set of symbols
} deriving (Eq,Show)

fromList :: [Sym] -> UnionType
fromList = fromSet . IS.fromList

fromAscList :: [Sym] -> UnionType
fromAscList = fromSet . IS.fromAscList

toList :: UnionType -> [Sym]
toList = toAscList

toAscList :: UnionType -> [Sym]
toAscList = IS.toAscList . set

-- | Construction from a set of symbols
fromSet :: IntSet -> UnionType
fromSet ss = U (IS.size ss) ss

-- | Is the symbol a member of the union?
member :: Sym -> UnionType -> Bool
member s = IS.member s . set

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: UnionType
bot = U 0 IS.empty

-- | Top (full) type given a size
top :: Int -> UnionType
top n = U (max 0 n) $ IS.fromDistinctAscList [0..n-1]

-- | Subtype relation (partial order)
leq :: UnionType -> UnionType -> Bool
leq (U n ss) (U n' ss') =
  n <= n' -- short circuit
  && ss `IS.isSubsetOf` ss'

-- | Strict subtype relation (partial order)
lt :: UnionType -> UnionType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: UnionType -> UnionType -> UnionType
join (U _ ss) (U _ ss') = fromSet $ IS.union ss ss'

-- | Greatest lower bound
meet :: UnionType -> UnionType -> UnionType
meet (U _ ss) (U _ ss') = fromSet $ IS.intersection ss ss'

-----------------
-- INFORMATION --
-----------------

-- NOTE: we could assume each element appears at least once in the
-- resolution and shrink the code by like ~O(n), but it's probably too
-- early for that

-- | Refine a type with a bit mask
refine :: UnionType -> BitVec -> UnionType
refine (U n ss) bv
  | len /= n = err $ "refine: bitvec length mismatch: " ++ show len
               ++ " should be " ++ show n
  | otherwise = fromSet $ IS.fromAscList
                [ s | (True, s) <- zip (BV.toBits bv) (IS.toAscList ss)]
  where len = BV.length bv

-- | Codelen (bits) of a refinement
refineLen :: UnionType -> Int
refineLen = length

-- | For encoding/decoding
bases :: Int -> UnionType -> [Integer]
bases k = replicate k . fromIntegral . length

-- | For decoding
fromIdxs :: UnionType -> [Integer] -> [Sym]
fromIdxs (U n ss) = fmap $ (v UV.!) . fromInteger
  where v = UV.fromListN n $ IS.toAscList ss

-- | For a count, a type and a code, instantiate the type into specific
-- symbols. Code is simply the index in the cartesian power `k` of the
-- union. Returns Nothing if the given code is not long enough to fully
-- specify a resolution. If code is long enough, returns the rest of the
-- code with the resolution's code removed.
resolve :: Int -> UnionType -> BitVec -> Maybe ([Sym], BitVec)
resolve k u bv = first (fromIdxs u) <$> Variety.decode (bases k u) bv

-- | k log(n)
resolveInfo :: Int -> UnionType -> Double
resolveInfo k (U n _) = fromIntegral k * log2 base
  where base = fromIntegral n

-- | Length of a code to instantiate a type into `k` symbols
resolveLen :: Int -> UnionType -> Int
resolveLen = ceiling .: resolveInfo

err :: String -> a
err = error . ("UnionType." ++)
