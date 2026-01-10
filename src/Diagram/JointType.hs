module Diagram.JointType (module Diagram.JointType) where

import Control.Monad.Random (MonadRandom)


import Data.Tuple.Extra
import qualified Data.List.Extra as L
import qualified Data.Map as M

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Model (Sym)
import Diagram.Joints (Joints)
import Diagram.UnionType (UnionType)
import qualified Diagram.UnionType as U
import qualified Diagram.Random as R
import Diagram.Information (log2)
import Diagram.Util

-- | A joint of unions
data JointType = JT {
  left :: !UnionType, -- s0s
  right :: !UnionType -- s1s
} deriving (Eq,Show)

fromJoints :: Joints -> JointType
fromJoints = uncurry JT . both U.fromList . unzip . M.keys

-- | Is the symbol a member of the union?
member :: (Sym,Sym) -> JointType -> Bool
member (s0,s1) (JT u0 u1) = U.member s0 u0 && U.member s1 u1

-- | (DO NOT USE (NAIVE)) Generate a random refinement of a type. The
-- produced type is not necessarily a valid refinement (least upper
-- bound) of the set of joints it covers.
genRefinement_ :: MonadRandom m => JointType -> m JointType
genRefinement_ (JT u0 u1) = do
  (_,ss0) <- R.split (U.toAscList u0)
  (_,ss1) <- R.split (U.toAscList u1)
  return $ JT (U.fromDistinctAscList ss0) (U.fromDistinctAscList ss1)

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: JointType
bot = JT U.bot U.bot

-- | Subtype relation (partial order)
leq :: JointType -> JointType -> Bool
leq (JT u0 u1) (JT u0' u1') =
  U.length u1 <= U.length u1' -- short-circuit
  && U.leq u0 u0'
  && U.leq u1 u1'

-- | Strict subtype relation (partial order)
lt :: JointType -> JointType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: JointType -> JointType -> JointType
join (JT u0 u1) (JT u0' u1') =
  JT (U.join u0 u0') (U.join u1 u1')

-- | Greatest lower bound
meet :: JointType -> JointType -> JointType
meet (JT u0 u1) (JT u0' u1') =
  JT (U.meet u0 u0') (U.meet u1 u1')

-----------------
-- INFORMATION --
-----------------

-- | Refine a type with a bit mask
refine :: JointType -> BitVec -> JointType
refine (JT u0 u1) bv
  | len /= n0 + n1 = err $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = JT (U.refine u0 bv0) (U.refine u1 bv1)
  where
    n0 = U.length u0
    n1 = U.length u1
    len = BV.length bv
    (bv0,bv1) = BV.splitAt n0 bv

-- | Codelen (bits) of a refinement
refineLen :: JointType -> Int
refineLen (JT u0 u1) = U.refineLen u0 + U.refineLen u1

-- | For encoding/decoding, [n0,n1,n0,n1,n0,n1,...]
bases :: Int -> JointType -> [Integer]
bases k (JT u0 u1) = concat $
  L.transpose [U.bases k u0, U.bases k u1]

-- | For decoding
fromIdxs :: JointType -> [Integer] -> [(Sym,Sym)]
fromIdxs (JT u0 u1) is = zip s0s s1s
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
resolveInfo k (JT u0 u1) = fromIntegral k * log2 base
  where base = fromIntegral (U.length u0 * U.length u1)

-- | Length of a code to instantiate a type into `k` constructions
resolveLen :: Int -> JointType -> Int
resolveLen = ceiling .: resolveInfo

err :: String -> a
err = error . ("JointType." ++)
