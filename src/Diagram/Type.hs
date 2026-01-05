module Diagram.Type (module Diagram.Type) where

import Data.Bifunctor
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as U
import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Model (Sym)
import Diagram.Information

-- | A joint of two unions :: (T0,T1)
data Type = T !Int  -- n0  :: left union size
            !IntSet -- s0s :: left union
            !Int    -- n1  :: right union size
            !IntSet -- s1s :: right union
  deriving (Eq,Show)

-- | Construction from a set of left symbols and a set of right symbols
fromSets :: IntSet -> IntSet -> Type
fromSets s0s s1s = T n0 s0s n1 s1s
  where n0 = IS.size s0s
        n1 = IS.size s1s

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: Type
bot = T 0 IS.empty 0 IS.empty

-- | Subtype relation (partial order)
leq :: Type -> Type -> Bool
leq (T n0 s0s n1 s1s) (T n0' s0s' n1' s1s') =
  n0 <= n0'    -- short circuit
  && n1 <= n1' -- short circuit
  && s0s `IS.isSubsetOf` s0s'
  && s1s `IS.isSubsetOf` s1s'

-- | Strict subtype relation (partial order)
lt :: Type -> Type -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: Type -> Type -> Type
join (T _ s0s _ s1s) (T _ s0s' _ s1s') =
  fromSets (IS.union s0s s0s') (IS.union s1s s1s')

-- | Greatest lower bound
meet :: Type -> Type -> Type
meet (T _ s0s _ s1s) (T _ s0s' _ s1s') =
  fromSets (IS.intersection s0s s0s') (IS.intersection s1s s1s')

-----------------
-- INFORMATION --
-----------------

-- | Refine a type with a bit mask
refine :: Type -> BitVec -> Type
refine (T n0 s0s n1 s1s) bv
  | len /= n0 + n1 = error $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = fromSets s0s' s1s'
  where
    len = BV.length bv
    (b0s,b1s) = splitAt n0 $ BV.toBits bv
    s0s' = IS.fromAscList [ s | (True, s) <- zip b0s (IS.toAscList s0s)]
    s1s' = IS.fromAscList [ s | (True, s) <- zip b1s (IS.toAscList s1s)]

-- | Codelen (bits) of a refinement
refineLen :: Type -> Int
refineLen (T n0 _ n1 _) = n0 + n1

-- | For a joint count, a type and a code, instantiate the type into
-- specific joints. Code is simply the cartesian power-`k01` of both
-- left and right unions, interleaved left-right-left-right-etc. Returns
-- Nothing if the given code is not long enough to fully specify a
-- resolution. If code is long enough, returns the rest of the code with
-- the resolution's code removed.
resolve :: Int -> Type -> BitVec -> Maybe ([(Sym,Sym)], BitVec)
resolve k01 (T n0 s0s n1 s1s) bv = do -- Maybe
  (idxs, bv') <- Variety.decode bases bv
  let pairs = go $ fmap fromInteger idxs
      jts = bimap (v0 U.!) (v1 U.!) <$> pairs
  return (jts, bv')
  where
    bases = concat $ replicate k01 $ fromIntegral <$> [n0,n1]
    v0 = U.fromList $ IS.toAscList s0s
    v1 = U.fromList $ IS.toAscList s1s

    go [] = []
    go (a:b:rest) = (a,b) :go rest
    go _ = error "resolve: odd number of indexes produced"

-- | Length of a code to instantiate a type into `k01` constructions
resolveLen :: Int -> Type -> Int
resolveLen k01 (T n0 _ n1 _) = ceiling $ k01f * log2 base
  where base = fromIntegral (n0 * n1)
        k01f = fromIntegral k01
