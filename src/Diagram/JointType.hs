{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TupleSections, LambdaCase, TypeApplications #-}

module Diagram.JointType (module Diagram.JointType) where

import Control.Monad as Monad
import Control.Lens hiding (both,last1)
import Control.Monad.State.Strict
import Control.Monad.Random

import Data.Maybe
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Primitive
import Diagram.Model (Sym)
import Diagram.Joints (Joints)
import Diagram.UnionType (UnionType(..))
import qualified Diagram.UnionType as U
import qualified Diagram.Random as R
import Diagram.Information (log2)
import Diagram.Util

-- | A joint of unions
data JointType = JT {
  left :: !UnionType, -- s0s
  right :: !UnionType -- s1s
} deriving (Eq,Show)

size :: JointType -> (Int, Int)
size (JT u0 u1) = (U.length u0, U.length u1)

fromJoints :: Joints -> JointType
fromJoints = uncurry JT . both U.fromList . unzip . M.keys

-- | Is the symbol a member of the union?
member :: (Sym,Sym) -> JointType -> Bool
member (s0,s1) (JT u0 u1) = U.member s0 u0 && U.member s1 u1

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

----------------
-- REFINEMENT --
----------------

-- | State record to track the two maps and two sets
data RefinementState a = RefinementState {
  _jtsByFst :: !(Map Sym (Bool, Map Sym a)),
  _jtsBySnd :: !(Map Sym (Bool, Map Sym a)),
  -- NOTE: Map Int instead of IntMap because we want O(1) size
  -- and O(log n) elemAt.
  _fstUnion :: !IntSet,
  _sndUnion :: !IntSet
}
makeLenses ''RefinementState

-- | Generate a random refinement, given a set of joints indexed both
-- ways
genRefinement :: forall m a. (MonadRandom m, PrimMonad m) =>
                 Map Sym (Map Sym a) -> Map Sym (Map Sym a) -> m JointType
genRefinement byFst0 bySnd0 =
  evalStateT go $ RefinementState ((False,) <$> byFst0)
                                  ((False,) <$> bySnd0)
                                  IS.empty IS.empty
  where
    go :: StateT (RefinementState a) m JointType
    go = get >>= \case
      (RefinementState byFst bySnd u0 u1)
        | total == 0 -> return $ JT (U.fromSet u0) (U.fromSet u1) -- end
        | otherwise -> do
            i <- getRandomR (0, total-1) -- select a symbol
            b <- getRandom @_ @Bool -- include/exclude it in the ref
            if i < len0 then goIntroFst b (fst $ M.elemAt i byFst)
              else goIntroSnd b (fst $ M.elemAt (i - len0) bySnd)
            go -- rec
              where len0 = M.size byFst -- O(1)
                    len1 = M.size bySnd -- O(1)
                    total = len0 + len1

    deleteFind :: Ord k => k -> Map k b -> (b, Map k b)
    deleteFind = first fromJust
                 .: M.updateLookupWithKey (\_ _ -> Nothing)

    -- | Eliminate a symbol, given its current entry in the byFst map,
    -- and enforce invariants whether it has be `sel`ected or not
    goIntroFst :: Bool -> Int -> StateT (RefinementState a) m ()
    goIntroFst sel s0 = do
      (_, m1) <- jtsByFst %%= deleteFind s0 -- remove from avail.
      when sel $ fstUnion %= IS.insert s0 -- add to result
      forM_ (M.keys m1) $ \s1 -> do -- remove from its joints
        -- Monad.join runs the cont/rec call :: m (m a) -> m a
        Monad.join $ jtsBySnd . at s1 %%= \case
          Nothing -> error "impossible"
          Just (free1, m0) ->
            ( cont -- cont/rec call returned bc %%= wants a pure fn
            , (free1', ) <$> nothingIf M.null m0' ) -- insert/delete
            where
              m0' = M.delete s0 m0
              free1' = sel || free1
              cont = when (M.size m0' == 1 && not free1') $
                     getRandom >>= flip goIntroFst last0
              last0 = head $ M.keys m0'

    -- | Converse as above, could probably be factored into one, but
    -- that might be too much
    goIntroSnd :: Bool -> Int -> StateT (RefinementState a) m ()
    goIntroSnd sel s1 = do
      (_, m0) <- jtsBySnd %%= deleteFind s1 -- remove from avail.
      when sel $ sndUnion %= IS.insert s1 -- add to result
      forM_ (M.keys m0) $ \s0 -> do -- remove from its joints
        -- Monad.join runs the cont/rec call :: m (m a) -> m a
        Monad.join $ jtsByFst . at s0 %%= \case
          Nothing -> error "impossible"
          Just (free0, m1) ->
            ( cont -- cont/rec call returned bc %%= wants a pure fn
            , (free0', ) <$> nothingIf M.null m1' ) -- insert/delete
            where
              m1' = M.delete s1 m1
              free0' = sel || free0
              cont = when (M.size m1' == 1 && not free0') $
                     getRandom >>= flip goIntroSnd last1
              last1 = head $ M.keys m1'

-- | (DO NOT USE (NAIVE)) Generate a random refinement of a type. The
-- produced type is not necessarily a valid refinement (least upper
-- bound) of the set of joints it covers.
genRefinement_ :: MonadRandom m => JointType -> m JointType
genRefinement_ (JT u0 u1) = do
  (_,ss0) <- R.split (U.toAscList u0)
  (_,ss1) <- R.split (U.toAscList u1)
  return $ JT (U.fromDistinctAscList ss0) (U.fromDistinctAscList ss1)
