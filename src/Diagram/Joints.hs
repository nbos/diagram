{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
module Diagram.Joints (module Diagram.Joints, Sym) where

import Control.Monad
import Control.Monad.ST (runST)

import Data.Function (on)
import Data.Tuple.Extra ((&&&))
import Data.Bifunctor (Bifunctor(second, first))
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Data.Vector.Mutable as MV

import Diagram.String

-- | Count and location of each candidate/joint symbol in the string
type Joints a = Map (Sym,Sym) a

size :: Joints a -> Int
size = M.size

-----------------
-- RE-INDEXING --
-----------------

data Joints2 a = J2                       -----------------------
  { byFst2 :: !(IntMap (IntMap a))        -- TWO-WAYS INT MAPS --
  , bySnd2 :: !(IntMap (IntMap a)) }      -----------------------
  deriving(Eq,Show)

-- | Double up the index given the number of symbols (max s1 + 1)
doubleIndex :: Int -> Joints a -> Joints2 a
doubleIndex m jts = J2 (byFst jts) (bySnd m jts)

data Joints2S a = J2S                     -------------------------
  { byFst2S :: !(Map Sym (Map Sym a))     -- TWO-WAYS SIZED MAPS --
  , bySnd2S :: !(Map Sym (Map Sym a)) }   -------------------------

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
