{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Diagram.Joints (module Diagram.Joints, Sym) where

import Control.Monad
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.ST

import Data.Function
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Set as Set

import qualified Data.Vector.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)

import Streaming hiding (first,second)
import qualified Streaming.Prelude as S

import Diagram.UnionType (Sym)
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D
import Diagram.Util

-- | Count and location of each candidate/joint symbol in the string
type Joints a = Map (Sym,Sym) a

size :: Joints a -> Int
size = M.size

------------------
-- CONSTRUCTION --
------------------

fromList :: [Sym] -> Joints (Int, IntSet)
fromList = fst . runIdentity . fromStream . S.each . zip [0..]

type Doubly s = D.Doubly MVector s Sym
-- | Construction using the indices of the doubly-linked list
fromDoubly :: PrimMonad m => Doubly (PrimState m) -> m (Joints (Int, IntSet))
fromDoubly = fmap fst
             . fromStream_ M.empty
             . D.streamWithKey

fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Joints (Int, IntSet), r)
fromStream = fromStream_ M.empty

fromStream_ :: Monad m => Joints (Int, IntSet) ->
               Stream (Of (Index,Sym)) m r -> m (Joints (Int, IntSet), r)
fromStream_ m0 iss0 = (S.next iss0 >>=) $ \case
  Left r -> return (m0, r)
  Right (i0s0,iss0') -> fromStreamOdd_ i0s0 m0 iss0'

fromStreamOdd_ :: Monad m => (Index, Sym) -> Joints (Int, IntSet) ->
                  Stream (Of (Index, Sym)) m r -> m (Joints (Int, IntSet), r)
fromStreamOdd_ (i0,s0) !m iss = (S.next iss >>=) $ \case
  Left r -> return (m,r) -- end
  Right (i1s1@(_,s1),ss') -> (S.next ss' >>=) $ \case
    Left r -> return (m', r) -- last joint
    Right (is2@(_,s2),ss'') -> f m' $ S.yield is2 >> ss''
      where f | s0 == s1 && s1 == s2 = fromStream_         -- even
              | otherwise            = fromStreamOdd_ i1s1 -- odd
    where m' = insert1 m (s0,s1) i0

----------------
-- OPERATIONS --
----------------

insert1 :: Joints (Int, IntSet) -> (Sym,Sym) -> Index -> Joints (Int, IntSet)
insert1 jts s0s1 i = M.insertWith f s0s1 (1, IS.singleton i) jts
  where f _ (n,is) = (n + 1, IS.insert i is)

-- | The union of two sets of counts + indices
union :: Joints (Int, IntSet) -> Joints (Int, IntSet) -> Joints (Int, IntSet)
union = M.unionWith $ \(a,s) (b,t) -> (a + b, IS.union s t)

-- | Subtract the counts + indices in the second map from the first map
difference :: Joints (Int, IntSet) -> Joints (Int, IntSet) -> Joints (Int, IntSet)
difference = M.mergeWithKey (const f) id id
  where f (a,s) (b,t) = nothingIf ((== 0) . fst) (a - b, IS.difference s t)

-----------------
-- RE-INDEXING --
-----------------

data Joints2 a = J2
  { byFst2 :: !(IntMap (IntMap a))
  , bySnd2 :: !(IntMap (IntMap a)) }

-- | Double up the index given the number of symbols (max s1 + 1)
doubleIndex :: Int -> Joints a -> Joints2 a
doubleIndex m jts = J2 (byFst jts) (bySnd m jts)

data Joints2S a = J2S
  { byFst2S :: !(Map Sym (Map Sym a))
  , bySnd2S :: !(Map Sym (Map Sym a)) }

sized :: Joints2 a -> Joints2S a
sized jts2 = J2S (im2m $ byFst2 jts2) (im2m $ bySnd2 jts2)

-- | Generic, given a `fromDistinctAscList` function
curryWith :: (forall a. [(Int,a)] -> m a) -> Map (Sym,Sym) s -> m (m s)
curryWith build = build
  . fmap (fst . fst . head &&& build . fmap (first snd))
  . L.groupBy ((==) `on` (fst . fst))
  . M.toAscList

-- | O(n) Convert a `(s0,s1) -> is` map into `s0 -> s1 -> is`
curry :: Map (Sym,Sym) a -> IntMap (IntMap a)
curry = curryWith IM.fromDistinctAscList

-- | O(n) Convert the `(s0,s1) -> is` map into `s0 -> s1 -> is`
byFst :: Map (Sym,Sym) a -> IntMap (IntMap a)
byFst = Diagram.Joints.curry

-- | O(n) Convert the `(s0,s1) -> is` map into `s0 -> s1 -> is`
byFstSized :: Map (Sym,Sym) a -> Map Int (Map Int a)
byFstSized = curryWith M.fromDistinctAscList

-- | Generic, given a `fromDistinctAscList` function
bySndWith :: (forall a. [(Int,a)] -> m a) ->
             Int -> Map (Sym,Sym) b -> m (m b)
bySndWith fromDistinctAscList numSymbols jts = runST $ do
  mv <- MV.replicate numSymbols []
  forM_ (M.toDescList jts) $ \((s0,s1),is) -> MV.modify mv ((s0,is):) s1
  ims <- MV.ifoldr (\s1 l -> if null l then id else
                       ((s1, fromDistinctAscList l):)) [] mv
  return $ fromDistinctAscList ims

-- | O(n + numSymbols) Given the number of symbols, convert the `(s0,s1)
-- -> is` map into `s1 -> s0 -> is`
bySnd :: Int -> Map (Sym,Sym) a -> IntMap (IntMap a)
bySnd = bySndWith IM.fromDistinctAscList

bySndSized :: Int -> Map (Sym,Sym) a -> Map Int (Map Int a)
bySndSized = bySndWith M.fromDistinctAscList

m2im :: Map Int (Map Int a) -> IntMap (IntMap a)
m2im = IM.fromDistinctAscList
       . fmap (second $ IM.fromDistinctAscList . M.toAscList)
       . M.toAscList

im2m :: IntMap (IntMap a) -> Map Int (Map Int a)
im2m = M.fromDistinctAscList
       . fmap (second $ M.fromDistinctAscList . IM.toAscList)
       . IM.toAscList

-----------
-- DEBUG --
-----------

-- | Re-compute the joint counts + locations to check the validity of a
-- given joints map. Throws an error if they differ.
validate :: PrimMonad m => Joints (Int, IntSet) -> Doubly (PrimState m) -> a -> m a
validate cdts ss a = do
  cdtsRef <- fromDoubly ss
  when (cdts /= cdtsRef) $
    let cdtsSet = Set.fromList $ M.toList cdts
        refSet = Set.fromList $ M.toList cdtsRef
    in error $ "Joints.validate:\n"
       ++ "should include: " ++ show (refSet Set.\\ cdtsSet) ++ "\n"
       ++ "not:            " ++ show (cdtsSet Set.\\ refSet)
  return a
