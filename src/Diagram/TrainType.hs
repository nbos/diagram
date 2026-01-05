module Diagram.TrainType (module Diagram.TrainType) where

import Data.Tuple.Extra
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import Codec.Arithmetic.Variety.BitVec (BitVec)

import Diagram.Joints (Joints)
import Diagram.Type (Type(T))
import qualified Diagram.Type as T

-- | A type with joints information for fast access during train
data TrainType = TT !Type -- :: (n0,s0s,n1,s1s)
                 !Int -- k01 :: joint count
                 !Joints -- :: joints
  deriving (Eq,Show)

-- | Construction from a set of Joints in a string
fromJoints :: Joints -> TrainType
fromJoints jts = TT (T.fromSets s0s s1s) (M.size jts) jts
  where (s0s,s1s) = both IS.fromList $ unzip $ M.keys jts

-- | Refine a type with a bit mask
refine :: TrainType -> BitVec -> TrainType
refine (TT t _ jts) bv = TT t' n' jts'
  where
    t'@(T _ s0s' _ s1s') = T.refine t bv
    jts' = M.filterKeys (\(s0,s1) -> s0 `IS.member` s0s' &&
                                     s1 `IS.member` s1s') jts
    n' = sum $ fst <$> jts'
