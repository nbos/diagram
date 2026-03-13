{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase #-}
module Diagram.Joints (module Diagram.Joints, Sym) where

import Control.Monad
import Control.Lens hiding (Index)
import Control.Monad.Primitive (PrimMonad(PrimState))
import Control.Monad.ST

import Data.Function
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.Set as Set
import Data.Strict.Tuple (Pair((:!:)))

import qualified Data.Vector.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)

import Streaming hiding (first,second)
import qualified Streaming.Prelude as S

import Diagram.UnionType (Sym)
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D

-- | Count and location of each candidate/joint symbol in the string
type Joints a = Map (Sym,Sym) a

size :: Joints a -> Int
size = M.size

data Sites = Sites
  {    _constructive :: !(Pair Int IntSet)
  , _nonconstructive :: !(Pair Int IntSet) }
  deriving(Eq,Ord,Show)
makeLenses ''Sites

noSites :: Sites
noSites = Sites no no
  where no = 0 :!: IS.empty

------------------
-- CONSTRUCTION --
------------------

fromList :: [Sym] -> Joints Sites
fromList = fst . runIdentity . fromStream . S.each . zip [0..]

type Doubly s = D.Doubly MVector s Sym
-- | Construction using the indices of the doubly-linked list
fromDoubly :: PrimMonad m =>
              Doubly (PrimState m) -> m (Joints Sites)
fromDoubly = fmap fst . fromStream . D.streamWithKey

fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Joints Sites, r)
fromStream iss0 = (S.next iss0 >>=) $ \case
  Left r -> return (M.empty, r)
  Right (i0s0,iss0') -> fromStream1 i0s0 M.empty iss0'

fromStream1 :: Monad m => (Index, Sym) -> Joints Sites ->
               Stream (Of (Index, Sym)) m r -> m (Joints Sites, r)
fromStream1 (i0,s0) m iss = (S.next iss >>=) $ \case
  Left r -> return (m,r) -- end
  Right (i1s1@(_,s1),ss') -> fromStream2 s0 i1s1 m' ss'
    where m' = addCons (s0,s1) i0 m -- inc constructive

-- | Joints from a stream that follows two symbols, given the index of
-- the last symbol too.
fromStream2 :: Monad m => Sym -> (Index, Sym) -> Joints Sites ->
               Stream (Of (Index, Sym)) m r -> m (Joints Sites, r)
fromStream2 sm1 (i0,s0) !m iss = (S.next iss >>=) $ \case
  Left r -> return (m,r) -- end
  Right (i1s1@(_,s1),ss') -> cont i1s1 m' ss'
    where m' = add (s0,s1) i0 m
          (add,cont)
            | sm1 == s0 && s0 == s1 = (addNoncons, fromStream1)
            | otherwise             = (addCons, fromStream2 s0)

addCons :: (Sym,Sym) -> Index -> Joints Sites -> Joints Sites
addCons (s0,s1) i0 = at (s0,s1) . non noSites . constructive
                     %~ ((+1) `bimap` IS.insert i0)

addNoncons :: (Sym,Sym) -> Index -> Joints Sites -> Joints Sites
addNoncons (s0,s1) i0 = at (s0,s1) . non noSites . nonconstructive
                        %~ ((+1) `bimap` IS.insert i0)

-----------------
-- RE-INDEXING --
-----------------

data Joints2 a = J2
  { byFst2 :: !(IntMap (IntMap a))
  , bySnd2 :: !(IntMap (IntMap a)) }
  deriving(Eq,Show)

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
validate :: PrimMonad m => Joints Sites -> Doubly (PrimState m) -> a -> m a
validate cdts ss a = do
  cdtsRef <- fromDoubly ss
  when (cdts /= cdtsRef) $
    let cdtsSet = Set.fromList $ M.toList cdts
        refSet = Set.fromList $ M.toList cdtsRef
    in error $ "Joints.validate:\n"
       ++ "should include: " ++ show (refSet Set.\\ cdtsSet) ++ "\n"
       ++ "not:            " ++ show (cdtsSet Set.\\ refSet)
  return a
