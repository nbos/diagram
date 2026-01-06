module Diagram.JointType (module Diagram.JointType) where

import Data.Bifunctor
import qualified Data.List.Extra as L

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Model (Sym)
import Diagram.UnionType (UnionType)
import qualified Diagram.UnionType as U
import Diagram.Information (log2)
import Diagram.Util

-- | A joint of unions
data JointType = J {
  left :: !UnionType, -- s0s
  right :: !UnionType -- s1s
} deriving (Eq,Show)

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: JointType
bot = J U.bot U.bot

-- | Subtype relation (partial order)
leq :: JointType -> JointType -> Bool
leq (J u0 u1) (J u0' u1') =
  U.length u1 <= U.length u1' -- short-circuit
  && U.leq u0 u0'
  && U.leq u1 u1'

-- | Strict subtype relation (partial order)
lt :: JointType -> JointType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: JointType -> JointType -> JointType
join (J u0 u1) (J u0' u1') =
  J (U.join u0 u0') (U.join u1 u1')

-- | Greatest lower bound
meet :: JointType -> JointType -> JointType
meet (J u0 u1) (J u0' u1') =
  J (U.meet u0 u0') (U.meet u1 u1')

-----------------
-- INFORMATION --
-----------------

-- | Refine a type with a bit mask
refine :: JointType -> BitVec -> JointType
refine (J u0 u1) bv
  | len /= n0 + n1 = err $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = J (U.refine u0 bv0) (U.refine u1 bv1)
  where
    n0 = U.length u0
    n1 = U.length u1
    len = BV.length bv
    (bv0,bv1) = BV.splitAt n0 bv

-- | Codelen (bits) of a refinement
refineLen :: JointType -> Int
refineLen (J u0 u1) = U.refineLen u0 + U.refineLen u1

-- | For encoding/decoding, [n0,n1,n0,n1,n0,n1,...]
bases :: Int -> JointType -> [Integer]
bases k (J u0 u1) = concat $
  L.transpose [U.bases k u0, U.bases k u1]

-- | For decoding
fromIdxs :: JointType -> [Integer] -> [(Sym,Sym)]
fromIdxs (J u0 u1) is = zip s0s s1s
  where
    s0s = U.fromIdxs u0 is0
    s1s = U.fromIdxs u1 is1
    (is0,is1) = case L.transpose (L.chunksOf 2 is) of
                  [l0,l1] -> (l0,l1)
                  _else -> err "fromIdxs: impossible"

-- | For a joint count, a type and a code, instantiate the type into
-- specific joints. Returns Nothing if the given code is not long enough
-- to fully specify a resolution. If code is long enough, returns the
-- rest of the code with the resolution's code removed.
resolve :: Int -> JointType -> BitVec -> Maybe ([(Sym,Sym)], BitVec)
resolve k t bv = first (fromIdxs t) <$> Variety.decode (bases k t) bv

-- | k log(n0*n1)
resolveInfo :: Int -> JointType -> Double
resolveInfo k (J u0 u1) = fromIntegral k * log2 base
  where base = fromIntegral (U.length u0 * U.length u1)

-- | Length of a code to instantiate a type into `k` constructions
resolveLen :: Int -> JointType -> Int
resolveLen = ceiling .: resolveInfo

err :: String -> a
err = error . ("JointType." ++)
