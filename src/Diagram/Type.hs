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
data Type = T !Int !IntSet !Int !IntSet
  deriving (Eq,Show)

fromSets :: IntSet -> IntSet -> Type
fromSets s0s s1s = T n0 s0s n1 s1s
  where n0 = IS.size s0s
        n1 = IS.size s1s

-- | Refine a type with a mask
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

refineLen :: Type -> Int
refineLen (T n0 _ n1 _) = n0 + n1

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

resolveLen :: Int -> Type -> Int
resolveLen k01 (T n0 _ n1 _) = ceiling $ k01f * log2 base
  where base = fromIntegral (n0 * n1)
        k01f = fromIntegral k01
