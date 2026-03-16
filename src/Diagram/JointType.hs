{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE InstanceSigs, DeriveGeneric, DeriveAnyClass #-}

module Diagram.JointType (module Diagram.JointType, Sym) where

import GHC.Generics (Generic)
import Control.Lens hiding (both)

import Data.Hashable
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Joints (Joints)
import Diagram.UnionType (Sym,UnionType(..))
import qualified Diagram.UnionType as UT
import Diagram.Information

import Diagram.Util

-- | A joint of unions
data JointType = JT {
  _left :: !UnionType, -- s0s
  _right :: !UnionType -- s1s
} deriving (Eq,Generic,Hashable)
makeLenses ''JointType

instance Show JointType where
  show :: JointType -> String
  show jt = "fromLists " ++ show u0 ++ " " ++ show u1
    where (u0,u1) = toLists jt

size :: JointType -> (Int, Int)
size (JT u0 u1) = (UT.size u0, UT.size u1)

fromJoints :: Joints a -> JointType
fromJoints = uncurry JT . both UT.fromList . unzip . M.keys

fromLists :: [Sym] -> [Sym] -> JointType
fromLists syms0 syms1 = JT (UT.fromList syms0) (UT.fromList syms1)

toLists :: JointType -> ([Sym],[Sym])
toLists (JT u0 u1) = (UT.toAscList u0, UT.toAscList u1)

-- | Is the symbol a member of the union?
member :: (Sym,Sym) -> JointType -> Bool
member (s0,s1) (JT u0 u1) = UT.member s0 u0 && UT.member s1 u1

-- | Safe left insertion
insertLeft :: Sym -> JointType -> JointType
insertLeft s (JT u0 u1) = JT (UT.insert s u0) u1

-- | Unsafe left insertion. Breaks invariant if the symbol is already
-- present in left union.
insertLeftMissing :: Sym -> JointType -> JointType
insertLeftMissing s (JT u0 u1) = JT (UT.insertMissing s u0) u1

-- | Safe right insertion
insertRight :: Sym -> JointType -> JointType
insertRight s (JT u0 u1) = JT u0 (UT.insert s u1)

-- | Unsafe right insertion. Breaks invariant if the symbol is already
-- present in right union.
insertRightMissing :: Sym -> JointType -> JointType
insertRightMissing s (JT u0 u1) = JT u0 (UT.insertMissing s u1)

-- | Safe both left/right insertion at once
insertBoth :: Sym -> Sym -> JointType -> JointType
insertBoth s0 s1 (JT u0 u1) = JT (UT.insert s0 u0) (UT.insert s1 u1)

-- | Unsafe both left/right insertion at once. Breaks invariant if the
-- symbols are already present in the respective unions.
insertBothMissing :: Sym -> Sym -> JointType -> JointType
insertBothMissing s0 s1 (JT u0 u1) = JT u0' u1'
  where u0' = UT.insertMissing s0 u0
        u1' = UT.insertMissing s1 u1

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: JointType
bot = JT UT.bot UT.bot

-- | Subtype relation (partial order)
leq :: JointType -> JointType -> Bool
leq (JT u0 u1) (JT u0' u1') =
  UT.size u1 <= UT.size u1' -- short-circuit
  && UT.leq u0 u0'
  && UT.leq u1 u1'

-- | Strict subtype relation (partial order)
lt :: JointType -> JointType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: JointType -> JointType -> JointType
join (JT u0 u1) (JT u0' u1') =
  JT (UT.join u0 u0') (UT.join u1 u1')

-- | Greatest lower bound
meet :: JointType -> JointType -> JointType
meet (JT u0 u1) (JT u0' u1') =
  JT (UT.meet u0 u0') (UT.meet u1 u1')

-----------
-- CODEC --
-----------

-- | Refine a type with a bit mask
refine :: JointType -> BitVec -> JointType
refine (JT u0 u1) bv
  | len /= n0 + n1 = err $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = JT (UT.refine u0 bv0) (UT.refine u1 bv1)
  where
    n0 = UT.size u0
    n1 = UT.size u1
    len = BV.length bv
    (bv0,bv1) = BV.splitAt n0 bv

-- | For a joint count, a type and a code, instantiate the type into
-- specific joints. Returns Nothing if the given code is not long enough
-- to fully specify a resolution. If code is long enough, returns the
-- rest of the code with the resolution's code removed.
resolve :: Int -> JointType -> BitVec -> Maybe ([(Sym,Sym)], BitVec)
resolve k t bv = first (resolveIndexes t) <$> Variety.decode (bases k t) bv

-- | For encoding/decoding, [n0,n1,n0,n1,n0,n1,...]
bases :: Int -> JointType -> [Integer]
bases k (JT u0 u1) = concat $
  L.transpose [UT.bases k u0, UT.bases k u1]

-- | For decoding
resolveIndexes :: JointType -> [Integer] -> [(Sym,Sym)]
resolveIndexes (JT u0 u1) is = zip s0s s1s
  where
    s0s = UT.fromIdxs u0 is0
    s1s = UT.fromIdxs u1 is1
    (is0,is1) = case L.transpose (L.chunksOf 2 is) of
                  [l0,l1] -> (l0,l1)
                  _else -> err "resolveIndexes: impossible"

err :: String -> a
err = error . ("JointType." ++)

-----------------
-- INFORMATION --
-----------------

-- | Codelen (bits) of a refinement
refineLen :: JointType -> Int
refineLen (JT u0 u1) = UT.refineLen u0 + UT.refineLen u1

-- | O(1) Number of different joints covered by the type
variety :: JointType -> Int
variety (JT u0 u1) = UT.size u0 * UT.size u1

-- | k log(n0*n1)
resolutionInfo :: Int -> JointType -> Double
resolutionInfo k jt = fromIntegral k * log2 base
  where base = fromIntegral $ variety jt

-- | Length of a code to instantiate a type into `k` constructions
resolutionLen :: Int -> JointType -> Int
resolutionLen = ceiling .: resolutionInfo

