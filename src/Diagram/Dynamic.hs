module Diagram.Dynamic (module Diagram.Dynamic) where

import Prelude hiding (read, length, replicate, null, mapM_)
import Control.Monad.Primitive (PrimMonad(PrimState))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MV
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Mutable as Boxed
import qualified Data.Vector.Unboxed.Mutable as Unboxed
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Primitive.Mutable as Primitive

-- | 'Dynamic' is a resizable vector based on mutable vector @v@.
-- It keeps track of length and capacity separately.
data Dynamic m v a = Dynamic {
  dynLength :: !Int,
  dynVector :: !(v (PrimState m) a)
}

type BoxedVec     m = Dynamic m Boxed.MVector
type UnboxedVec   m = Dynamic m Unboxed.MVector
type StorableVec  m = Dynamic m Storable.MVector
type PrimitiveVec m = Dynamic m Primitive.MVector

-- | Create an empty vector with the given number of pre-allocated
-- elements.
withCapacity :: (PrimMonad m, MVector v a) => Int -> m (Dynamic m v a) 
withCapacity = fmap (Dynamic 0) . MV.new
{-# INLINE withCapacity #-}

-- | Create an empty vector
new :: (PrimMonad m, MVector v a) => m (Dynamic m v a)
new = withCapacity 0
{-# INLINE new #-}

-- | Clone the given vector
clone :: (PrimMonad m, MVector v a) => Dynamic m v a -> m (Dynamic m v a)
clone (Dynamic len vec) = Dynamic len <$> MV.clone vec
{-# INLINE clone #-}

-- | Create a vector and fill with the initial value.
replicate :: (PrimMonad m, MVector v a) =>
             Int -> a -> m (Dynamic m v a)
replicate len = fmap (Dynamic len) . MV.replicate len
{-# INLINE replicate #-}

-- | Like 'replicate', but initialises the elements by running the
-- action repeatedly
replicateM :: (PrimMonad m, MVector v a) => Int -> m a -> m (Dynamic m v a)
replicateM len = fmap (Dynamic len) . MV.replicateM len
{-# INLINE replicateM #-}

-- | Append an element to the vector
push :: (PrimMonad m, MVector v a) =>
        Dynamic m v a -> a -> m (Dynamic m v a)
push (Dynamic len vec) val = do
  vec' <- if MV.length vec == len then MV.unsafeGrow vec (max 1 len)
          else return vec
  MV.write vec' len val
  return $ Dynamic (len + 1) vec'
{-# INLINE push #-}

-- | Pop the last element. Returns 'Nothing' if the vector is empty.
pop :: (PrimMonad m, MVector v a) =>
       Dynamic m v a -> m (Maybe a, Dynamic m v a)
pop dyn@(Dynamic len vec)
  | len == 0  = return (Nothing, dyn)
  | otherwise = do val <- MV.unsafeRead vec (len - 1)
                   return (Just val, Dynamic (len - 1) vec)
{-# INLINE pop #-}

-- | Convert a Dynamic vector to a list
toList :: (G.Vector v a, PrimMonad m) => Dynamic m (G.Mutable v) a -> m [a]
toList = MV.foldr' (:) [] . dynVector
{-# INLINE toList #-}

-- | Create a Dynamic vector from a list
fromList :: (G.Vector v a, PrimMonad m) =>
            [a] -> m (Dynamic m (G.Mutable v) a)
fromList = thaw . G.fromList
{-# INLINE fromList #-}

-- | Get the length of the vector.
length :: Dynamic v s a -> Int
length = dynLength
{-# INLINE length #-}

-- | Returns 'True' if the vector is empty
null :: Dynamic v s a -> Bool
null = (== 0) . dynLength
{-# INLINE null #-}

-- | Read an element at the given position.
read :: (PrimMonad m, MVector v a) => Dynamic m v a -> Int -> m a
read = MV.read . dynVector
{-# INLINE read #-}

-- | Safely read an element at the given position. Returns Nothing if
-- the index is out of bounds
readMaybe :: (PrimMonad m, MVector v a) => Dynamic m v a -> Int -> m (Maybe a)
readMaybe = MV.readMaybe . dynVector
{-# INLINE readMaybe #-}

-- | Write an element at the given position
write :: (PrimMonad m, MVector v a) => Dynamic m v a -> Int -> a -> m ()
write = MV.write . dynVector
{-# INLINE write #-}

-- | Modify an element at the given position
modify :: (PrimMonad m, MVector v a) =>
          Dynamic m v a -> (a -> a) -> Int -> m ()
modify = MV.modify . dynVector
{-# INLINE modify #-}

-- | Thaw an immutable vector and create a 'Dynamic' one.
thaw :: (G.Vector v a, PrimMonad m) =>
        v a -> m (Dynamic m (G.Mutable v) a)
thaw v = Dynamic (G.length v) <$> G.thaw v
{-# INLINE thaw #-}

-- | Take a snapshot of a 'Dynamic' vector.
freeze :: (G.Vector v a, PrimMonad m) =>
          Dynamic m (G.Mutable v) a -> m (v a)
freeze (Dynamic len vec) = do
  v <- G.freeze vec
  return $! G.unsafeTake len v
{-# INLINE freeze #-}

-- | Take a snapshot of a 'Dynamic' vector. The original vector may not
-- be used.
unsafeFreeze :: (G.Vector v a, PrimMonad m) =>
                Dynamic m (G.Mutable v) a -> m (v a)
unsafeFreeze (Dynamic len vec) = do
  v <- G.unsafeFreeze vec
  return $! G.unsafeTake len v
{-# INLINE unsafeFreeze #-}

-- | Turn 'Dynamic' vector into a regular mutable vector.
fromDynamic :: (PrimMonad m, MVector v a) =>
               Dynamic m v a -> v (PrimState m) a
fromDynamic (Dynamic len vec) = MV.unsafeTake len vec
{-# INLINE fromDynamic #-}

-- | Create a 'Dynamic' vector from a mutable vector.
toDynamic :: (PrimMonad m, MVector v a) =>
             v (PrimState m) a -> Dynamic m v a
toDynamic vec = Dynamic (MV.length vec) vec
{-# INLINE toDynamic #-}

-----------
-- FOLDS --
-----------

-- | O(n) Apply the monadic action to every element of the vector,
-- discarding the results.
mapM_ :: (PrimMonad m, MVector v a) => (a -> m b) -> Dynamic m v a -> m ()
mapM_ f = MV.mapM_ f . fromDynamic
{-# INLINE mapM_ #-}

-- | O(n) Apply the monadic action to every element of the vector and
-- its index, discarding the results.
imapM_ :: (PrimMonad m, MVector v a) => (Int -> a -> m b) -> Dynamic m v a -> m ()
imapM_ f = MV.imapM_ f . fromDynamic
{-# INLINE imapM_ #-}

-- | O(n) Apply the monadic action to every element of the vector,
-- discarding the results.  It's the same as flip mapM_.
forM_ :: (PrimMonad m, MVector v a) => Dynamic m v a -> (a -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

-- | O(n) Apply the monadic action to every element of the vector and
-- its index, discarding the results.  It's the same as flip imapM_.
iforM_ :: (PrimMonad m, MVector v a) => Dynamic m v a -> (Int -> a -> m b) -> m ()
iforM_ = flip imapM_
{-# INLINE iforM_ #-}

-- Left folds

-- | O(n) Pure left fold.
foldl :: (PrimMonad m, MVector v a) =>
         (b -> a -> b) -> b -> Dynamic m v a -> m b
foldl f z = MV.foldl f z . fromDynamic
{-# INLINE foldl #-}

-- | O(n) Pure left fold with strict accumulator.
foldl' :: (PrimMonad m, MVector v a) =>
          (b -> a -> b) -> b -> Dynamic m v a -> m b
foldl' f z = MV.foldl' f z . fromDynamic
{-# INLINE foldl' #-}

-- | O(n) Monadic fold.
foldM :: (PrimMonad m, MVector v a) =>
         (b -> a -> m b) -> b -> Dynamic m v a -> m b
foldM f z = MV.foldM f z . fromDynamic
{-# INLINE foldM #-}

-- | O(n) Monadic fold with strict accumulator.
foldM' :: (PrimMonad m, MVector v a) =>
          (b -> a -> m b) -> b -> Dynamic m v a -> m b
foldM' f z = MV.foldM' f z . fromDynamic
{-# INLINE foldM' #-}

-- Right folds

-- | O(n) Pure right fold.
foldr :: (PrimMonad m, MVector v a) =>
         (a -> b -> b) -> b -> Dynamic m v a -> m b
foldr f z = MV.foldr f z . fromDynamic
{-# INLINE foldr #-}

-- | O(n) Pure right fold with strict accumulator.
foldr' :: (PrimMonad m, MVector v a) =>
          (a -> b -> b) -> b -> Dynamic m v a -> m b
foldr' f z = MV.foldr' f z . fromDynamic
{-# INLINE foldr' #-}

-- | O(n) Monadic right fold.
foldrM :: (PrimMonad m, MVector v a) =>
          (a -> b -> m b) -> b -> Dynamic m v a -> m b
foldrM f z = MV.foldrM f z . fromDynamic
{-# INLINE foldrM #-}

-- | O(n) Monadic right fold with strict accumulator.
foldrM' :: (PrimMonad m, MVector v a) =>
           (a -> b -> m b) -> b -> Dynamic m v a -> m b
foldrM' f z = MV.foldrM' f z . fromDynamic
{-# INLINE foldrM' #-}

-- Indexed left folds

-- | O(n) Pure left fold using a function applied to each element and
-- its index.
ifoldl :: (PrimMonad m, MVector v a) =>
          (b -> Int -> a -> b) -> b -> Dynamic m v a -> m b
ifoldl f z = MV.ifoldl f z . fromDynamic
{-# INLINE ifoldl #-}

-- | O(n) Pure left fold with strict accumulator using a function
-- applied to each element and its index.
ifoldl' :: (PrimMonad m, MVector v a) =>
           (b -> Int -> a -> b) -> b -> Dynamic m v a -> m b
ifoldl' f z = MV.ifoldl' f z . fromDynamic
{-# INLINE ifoldl' #-}

-- | O(n) Monadic fold using a function applied to each element and its
-- index.
ifoldM :: (PrimMonad m, MVector v a) =>
          (b -> Int -> a -> m b) -> b -> Dynamic m v a -> m b
ifoldM f z = MV.ifoldM f z . fromDynamic
{-# INLINE ifoldM #-}

-- | O(n) Monadic fold with strict accumulator using a function applied
-- to each element and its index.
ifoldM' :: (PrimMonad m, MVector v a) =>
           (b -> Int -> a -> m b) -> b -> Dynamic m v a -> m b
ifoldM' f z = MV.ifoldM' f z . fromDynamic
{-# INLINE ifoldM' #-}

-- Indexed right folds

-- | O(n) Pure right fold using a function applied to each element and
-- its index.
ifoldr :: (PrimMonad m, MVector v a) =>
          (Int -> a -> b -> b) -> b -> Dynamic m v a -> m b
ifoldr f z = MV.ifoldr f z . fromDynamic
{-# INLINE ifoldr #-}

-- | O(n) Pure right fold with strict accumulator using a function
-- applied to each element and its index.
ifoldr' :: (PrimMonad m, MVector v a) =>
           (Int -> a -> b -> b) -> b -> Dynamic m v a -> m b
ifoldr' f z = MV.ifoldr' f z . fromDynamic
{-# INLINE ifoldr' #-}

-- | O(n) Monadic right fold using a function applied to each element
-- and its index.
ifoldrM :: (PrimMonad m, MVector v a) =>
           (Int -> a -> b -> m b) -> b -> Dynamic m v a -> m b
ifoldrM f z = MV.ifoldrM f z . fromDynamic
{-# INLINE ifoldrM #-}

-- | O(n) Monadic right fold with strict accumulator using a function
-- applied to each element and its index.
ifoldrM' :: (PrimMonad m, MVector v a) =>
            (Int -> a -> b -> m b) -> b -> Dynamic m v a -> m b
ifoldrM' f z = MV.ifoldrM' f z . fromDynamic
{-# INLINE ifoldrM' #-}
