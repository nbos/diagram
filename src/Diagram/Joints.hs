{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Diagram.Joints (module Diagram.Joints) where

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

import Streaming hiding (first)
import qualified Streaming.Prelude as S

import Diagram.Model (Sym)
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D
import Diagram.Util

-- | Count and location of each candidate/joint symbol in the string
type Joints = Map (Sym,Sym) (Int, IntSet)

size :: Joints -> Int
size = M.size

------------------
-- CONSTRUCTION --
------------------

fromList :: [Sym] -> Joints
fromList = fst . runIdentity . fromStream . S.each . zip [0..]

type Doubly s = D.Doubly MVector s Sym
-- | Construction using the indices of the doubly-linked list
fromDoubly :: PrimMonad m => Doubly (PrimState m) -> m Joints
fromDoubly = fmap fst
             . fromStream_ M.empty
             . D.streamWithKey

fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Joints, r)
fromStream = fromStream_ M.empty

fromStream_ :: Monad m => Joints -> Stream (Of (Index,Sym)) m r -> m (Joints, r)
fromStream_ m0 iss0 = (S.next iss0 >>=) $ \case
  Left r -> return (m0, r)
  Right (i0s0,iss0') -> fromStreamOdd_ i0s0 m0 iss0'

fromStreamOdd_ :: Monad m => (Index, Sym) -> Joints ->
                  Stream (Of (Index, Sym)) m r -> m (Joints, r)
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

insert1 :: Joints -> (Sym,Sym) -> Index -> Joints
insert1 jts s0s1 i = M.insertWith f s0s1 (1, IS.singleton i) jts
  where f _ (n,is) = (n + 1, IS.insert i is)

-- | The union of two sets of counts + indices
union :: Joints -> Joints -> Joints
union = M.unionWith $ \(a,s) (b,t) -> (a + b, IS.union s t)

-- | Subtract the counts + indices in the second map from the first map
difference :: Joints -> Joints -> Joints
difference = M.mergeWithKey (const f) id id
  where f (a,s) (b,t) = nothingIf ((== 0) . fst) (a - b, IS.difference s t)

-----------------
-- RE-INDEXING --
-----------------

-- | Generic, given a `fromDistinctAscList` function
curryWith :: (forall a. [(Int,a)] -> m a) -> Map (Int,Int) s -> m (m s)
curryWith build = build
  . fmap (fst . fst . head &&& build . fmap (first snd))
  . L.groupBy ((==) `on` (fst . fst))
  . M.toAscList

-- | O(n) Convert a `(s0,s1) -> is` map into `s0 -> s1 -> is`
curry :: Map (Int,Int) a -> IntMap (IntMap a)
curry = curryWith IM.fromDistinctAscList

-- | O(n) Convert the `(s0,s1) -> is` map into `s0 -> s1 -> is`
byFst :: Joints -> IntMap (IntMap (Int, IntSet))
byFst = Diagram.Joints.curry

-- | O(n) Convert the `(s0,s1) -> is` map into `s0 -> s1 -> is`
byFstSized :: Joints -> Map Int (Map Int (Int, IntSet))
byFstSized = curryWith M.fromDistinctAscList

-- | Generic, given a `fromDistinctAscList` function
bySndWith :: (forall a. [(Int,a)] -> m a) ->
             Int -> Joints -> m (m (Int, IntSet))
bySndWith build numSymbols jts = runST $ do
  mv <- MV.replicate numSymbols []
  forM_ (M.toDescList jts) $ \((s0,s1),is) -> MV.modify mv ((s0,is):) s1
  ims <- MV.ifoldr (\s1 l -> if null l then id else
                       ((s1, build l):)) [] mv
  return $ build ims

-- | O(n + numSymbols) Given the number of symbols, convert the `(s0,s1)
-- -> is` map into `s1 -> s0 -> is`
bySnd :: Int -> Joints -> IntMap (IntMap (Int, IntSet))
bySnd = bySndWith IM.fromDistinctAscList

bySndSized :: Int -> Joints -> Map Int (Map Int (Int, IntSet))
bySndSized = bySndWith M.fromDistinctAscList


-----------
-- DEBUG --
-----------

-- | Re-compute the joint counts + locations to check the validity of a
-- given joints map. Throws an error if they differ.
validate :: PrimMonad m => Joints -> Doubly (PrimState m) -> a -> m a
validate cdts ss a = do
  cdtsRef <- fromDoubly ss
  when (cdts /= cdtsRef) $
    let cdtsSet = Set.fromList $ M.toList cdts
        refSet = Set.fromList $ M.toList cdtsRef
    in error $ "Joints.validate:\n"
       ++ "should include: " ++ show (refSet Set.\\ cdtsSet) ++ "\n"
       ++ "not:            " ++ show (cdtsSet Set.\\ refSet)
  return a
