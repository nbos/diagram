{-# LANGUAGE LambdaCase, TupleSections #-}
-- | Doubly-linked list with random access
module Diagram.Doubly (module Diagram.Doubly) where

import Prelude hiding (read)
import qualified Debug.Trace as Trace

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Primitive (PrimMonad(PrimState))

import Data.Maybe
import qualified Data.List as L
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

import Streaming
import qualified Streaming.Prelude as S

import Diagram.Util

type Index = Int
data Doubly v s a = Doubly
  !(Maybe Index)       -- ^ head index
  ![Index]             -- ^ freed indexes
  !(v s a)             -- ^ elements
  !(U.MVector s Index) -- ^ previous indexes
  !(U.MVector s Index) -- ^ next indexes

checkIntegrity :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> r -> m r
checkIntegrity l@(Doubly _ free elems _ _) r = do
  fwd <- S.toList_ $ streamKeys l
  bwd <- S.toList_ $ revStreamKeys l

  when (reverse bwd /= fwd) $
    err' $ "BWD keys is not the reverse of FWD keys:\n"
    ++ "FWD: " ++ show fwd ++ "\nBWD: " ++ show bwd

  let keySet = IS.fromList fwd
      freeSet = IS.fromList free
      indexSet = IS.fromList [0..MV.length elems - 1]

  when (IS.size keySet /= length fwd) $
    err' $ "Key set contains duplicates: " ++ show fwd
  when (IS.size freeSet /= length free) $
    err' $ "Free set contains duplicates: " ++ show free
  when (IS.union keySet freeSet /= indexSet) $
    err' $ "Some keys missing absent from both key and free set: "
    ++ show (IS.toList $ (indexSet IS.\\ keySet) IS.\\ freeSet)
  unless (IS.null $ IS.intersection keySet freeSet) $
    err' $ "Key and free set intersect: "
    ++ show (IS.intersection keySet freeSet)

  -- [TODO]: now that unset indexes carry (-1) in prevs and nexts we can
  -- check this is the case for all indexes not in free

  -- verify value initialization
  as <- S.toList_ $ stream l
  return $ foldr ((.) . seq) id as r

  where err' = err "checkIntegrity"

-- | Monadic traceShow of the list
traceShow :: (Show a, PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m ()
traceShow l = toList l >>= flip Trace.traceShow (return ())

nullIdx :: Index
nullIdx = -1

-- | Allocate a new list of the given size with undefined values
new :: (PrimMonad m, MVector v a) => Int -> m (Doubly v (PrimState m) a)
new sz = do
  elems <- MV.new sz -- uninitialized
  prevs <- MV.replicate sz nullIdx
  nexts <- MV.replicate sz nullIdx
  return $ Doubly Nothing [0..sz-1] elems prevs nexts

capacity :: MVector v a => Doubly v s a -> Int
capacity (Doubly _ _ elems _ _) = MV.length elems

null :: Doubly v s a -> Bool
null (Doubly mi0 _ _ _ _) = isNothing mi0

full :: Doubly v s a -> Bool
full (Doubly _ free _ _ _) = L.null free

headKey :: Doubly v s a -> Maybe Index
headKey (Doubly mi0 _ _ _ _) = mi0

lastKey :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Maybe Index)
lastKey (Doubly Nothing _ _ _ _) = return Nothing
lastKey (Doubly (Just i0) _ _ prevs _) = Just <$> MV.read prevs i0

lastElem :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Maybe a)
lastElem l = lastKey l >>= mapM (read l)

member :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m Bool
member (Doubly _ _ _ prevs _) i =
  (MV.readMaybe prevs i >>=) $ \case
  Nothing -> return False -- out of bounds
  Just prv | prv < 0 -> return False -- undefined
           | otherwise -> return True

-- | Synonym for @read@
(!) :: (PrimMonad m, MVector v a) => Doubly v (PrimState m) a -> Index -> m a
(!) = read
infixl 9 !

-- | Synonym for @readMaybe@
(!?) :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m (Maybe a)
(!?) = readMaybe
infixl 9 !?

-- | Read an index into an elem
read :: (PrimMonad m, MVector v a) => Doubly v (PrimState m) a -> Index -> m a
read (Doubly _ _ elems _ _) = MV.read elems

-- | Read an index into an elem, returns Nothing if the index is
-- undefined or out of bounds
readMaybe :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m (Maybe a)
readMaybe l i = ifM (member l i)
                (Just <$> read l i) (return Nothing)

-- | Write at a given index the given elem
write :: (PrimMonad m, MVector v a) =>
         Doubly v (PrimState m) a -> Index -> a -> m ()
write (Doubly _ _ elems _ _) = MV.write elems

-- | Clone the data in the given list into a new list with `n`
-- additional free slots
growBy :: (PrimMonad m, MVector v a) =>
  Int -> Doubly v (PrimState m) a -> m (Doubly v (PrimState m) a)
growBy n (Doubly mi0 free elems prevs nexts) = do
  let len = MV.length elems
      len' = len + n
      free' = free ++ [len..len'-1]
  elems' <- MV.grow elems n
  prevs' <- MV.grow prevs n
  nexts' <- MV.grow nexts n
  numLoop len (len'-1) $ \i -> do
    MV.write prevs' i (-1)
    MV.write nexts' i (-1)
  return $ Doubly mi0 free' elems' prevs' nexts'

-- | Double the capacity of the list
grow :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Doubly v (PrimState m) a)
grow l = growBy (max 1 $ capacity l) l

-- | Construct a doubly-linked list from a singly-linked list
fromList :: (PrimMonad m, MVector v a) => [a] -> m (Doubly v (PrimState m) a)
fromList as = do
  let len = length as
  elems <- MV.new len
  forM_ (zip [0..] as) $ uncurry $ MV.write elems
  prevs <- U.unsafeThaw (U.fromList ((len-1):[0..len-2]))
  nexts <- U.unsafeThaw (U.fromList ([1..len-1]++[0]))
  return $ Doubly (Just 0) [] elems prevs nexts

-- | Construct a doubly-linked list from a stream of at most `n`
-- elements
fromStream :: (PrimMonad m, MVector v a) =>
  Int -> Stream (Of a) m r -> m (Doubly v (PrimState m) a, r)
fromStream n str = do
  elems <- MV.new n
  mLast :> rest <- S.effects $ S.mapM (uncurry $ MV.write elems) $
                   S.last $ S.copy $
                   S.zip (S.enumFrom 0) $
                   S.splitAt n str
  (S.next rest >>=) $ \case
    Right _ -> err "fromStream" $ "Given stream contains more than "
               ++ show n ++ " elements"
    Left r -> case mLast of
      Nothing -> (,r) <$> new n
      Just (iLast,_) -> do
        let nullIdxs = replicate (n - 1 - iLast) nullIdx
        prevs <- U.unsafeThaw (U.fromList (iLast:[0..iLast-1]++nullIdxs))
        nexts <- U.unsafeThaw (U.fromList ([1..iLast]++0:nullIdxs))
        return (Doubly (Just 0) [] elems prevs nexts, r)

-- | Read the doubly-linked list into a singly-linked list. Use toStream
-- to not @sequence@ the reads.Applicative
toList :: (PrimMonad m, MVector v a) => Doubly v (PrimState m) a -> m [a]
toList = S.toList_ . stream

-- | Return previous index if the given index is not the head. Assumes
-- the index is valid/defined and panics otherwise.
prev :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m (Maybe Index)
prev (Doubly Nothing _ _ _ _) _ = emptyErr "prev"
prev (Doubly (Just i0) _ _ prevs _) i
  | i == i0 = return Nothing -- head
  | otherwise = (MV.readMaybe prevs i >>=) $ \case
      Nothing -> oobErr "prev" (i, MV.length prevs)
      Just prv | prv < 0 -> undefErr "prev" i
               | otherwise -> return $ Just prv

-- | Return the index of the element preceeding the element at a given
-- index in the list. Does not check if the value is defined in the
-- list. Returns the last index when given the head index, i.e. cycles.
unsafePrev :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m Index
unsafePrev (Doubly _ _ _ prevs _) = MV.read prevs

-- | Return next index if the given index is not the head. Assumes the
-- index is valid/defined and panics otherwise.
next :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m (Maybe Index)
next (Doubly Nothing _ _ _ _) _ = emptyErr "next"
next (Doubly (Just i0) _ _ nexts _) i =
  (MV.readMaybe nexts i >>=) $ \case
  Nothing -> oobErr "next" (i, MV.length nexts)
  Just nxt | nxt < 0   -> undefErr "next" i
           | nxt == i0 -> return Nothing -- tail
           | otherwise -> return $ Just nxt

-- | Return the index of the element following the element at a given
-- index in the list. Does not check if the value is defined in the
-- list. Returns the last index when given the head index, i.e. cycles.
unsafeNext :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m Index
unsafeNext (Doubly _ _ _ _ nexts) = MV.read nexts

-- | Modify an element at a given index. Throws an error if index is
-- undefined
modify :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> (a -> a) -> Index -> m ()
modify (Doubly _ _ elems _ _) = MV.modify elems

-- | Delete the element at a given index and seam previous with
-- next. No-op if the index is undefined.
delete :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> m (Doubly v (PrimState m) a)
delete l@(Doubly Nothing _ _ _ _) _ = return l -- empty ==> empty
delete l@(Doubly mi0@(Just i0) free elems prevs nexts) i = do
    prv <- MV.read prevs i -- prev <- i
    if prv < 0 then return l else do
      MV.write prevs i (-1)  -- (-1) <- i
      nxt <- MV.read nexts i --         i -> next
      MV.write nexts i (-1)  --         i -> (-1)
      MV.write prevs nxt prv -- prev <-( )-- next
      MV.write nexts prv nxt -- prev --( )-> next
      let mi0' | i   == nxt = Nothing -- singleton ==> empty
               | i   == i0  = Just nxt
               | otherwise  = mi0
      return $ Doubly mi0' (i:free) elems prevs nexts

streamKeysOf :: (PrimMonad m, MVector v a, Eq a) =>
  Doubly v (PrimState m) a -> a -> Stream (Of Index) m ()
streamKeysOf (Doubly Nothing _ _ _ _) _ = return ()
streamKeysOf (Doubly (Just i0) _ elems _ nexts) a = go i0
  where
    go i = do
      a' <- lift $ MV.read elems i
      when (a' == a) $ S.yield i
      nxt <- lift $ MV.read nexts i
      when (nxt /= i0) $ go nxt

streamKeysOfJoint :: (PrimMonad m, MVector v a, Eq a) =>
  Doubly v (PrimState m) a -> (a,a) -> Stream (Of Index) m ()
streamKeysOfJoint (Doubly Nothing _ _ _ _) _ = return ()
streamKeysOfJoint (Doubly (Just i0) _ elems _ nexts) (a0,a1) = go i0
  where
    go i = do
      a <- lift $ MV.read elems i
      nxt <- lift $ MV.read nexts i
      when (nxt /= i0) $ do
        if a == a0 then do
          a' <- lift $ MV.read elems nxt -- read next
          if a' == a1
          then do S.yield i -- match!
                  nxt' <- lift $ MV.read nexts nxt
                  when (nxt' /= i0) $ go nxt'
          else go nxt
        else go nxt

-- | Substitute the element at the given index and the next with the
-- given element. Assumes the index is not of the last element.
subst2 :: (PrimMonad m, MVector v a) =>
  a -> Doubly v (PrimState m) a -> Index -> m (Doubly v (PrimState m) a)
subst2 s01 l i = modify l (const s01) i
                 >> unsafeNext l i >>= delete l

-- | Append an element at the begining of the list. Grows the structure
-- in case there are no free spaces.
cons :: (PrimMonad m, MVector v a) => -- TODO: return index?
  a -> Doubly v (PrimState m) a -> m (Doubly v (PrimState m) a)
cons a l = tryCons a l >>= maybe (grow l >>= cons a) return

-- | Try to append an element at the beginning of the list, if the
-- capacity allows it.
tryCons :: (PrimMonad m, MVector v a) => -- TODO: return index?
  a -> Doubly v (PrimState m) a -> m (Maybe (Doubly v (PrimState m) a))
tryCons _ (Doubly _ [] _ _ _) = return Nothing
tryCons a (Doubly Nothing (i:free) elems prevs nexts) = do
  MV.write elems i a
  MV.write nexts i i
  MV.write prevs i i
  return $ Just $ Doubly (Just i) free elems prevs nexts

tryCons a (Doubly (Just i0) (i:free) elems prevs nexts) = do
  -- i0 (old head)
  i_n <- MV.read prevs i0 -- get last
  MV.write prevs i0 i

  -- i_n (last)
  MV.write nexts i_n i

  -- i (new head)
  MV.write elems i a
  MV.write prevs i i_n
  MV.write nexts i i0

  return $ Just $ Doubly (Just i) free elems prevs nexts
{-# INLINABLE tryCons #-}

-- | Append an element to the end of the list. Grows the structure in
-- case there are no free spaces.
-- TODO: return index?
snoc :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> a -> m (Index, Doubly v (PrimState m) a)
snoc l a = trySnoc l a >>= maybe (grow l >>= flip snoc a) return

-- | Try to append an element to the end of the list, if the capacity
-- allows it. Returns the index of the appended element.
trySnoc :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> a -> m (Maybe (Index, Doubly v (PrimState m) a))
trySnoc (Doubly _ [] _ _ _) _ = return Nothing
trySnoc (Doubly Nothing (i:free) elems prevs nexts) a = do
  MV.write elems i a
  MV.write nexts i i
  MV.write prevs i i
  return $ Just (i, Doubly (Just i) free elems prevs nexts)

trySnoc (Doubly (Just i0) (i:free) elems prevs nexts) a = do
  -- i0 (head)
  i_n <- MV.read prevs i0 -- get last
  MV.write prevs i0 i

  -- i_n (old last)
  MV.write nexts i_n i

  -- i (new last)
  MV.write elems i a
  MV.write prevs i i_n
  MV.write nexts i i0

  return $ Just (i, Doubly (Just i0) free elems prevs nexts)
{-# INLINABLE trySnoc #-}

-- | Try to remove the first element from the list
tryUncons :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Maybe (a, Doubly v (PrimState m) a))
tryUncons (Doubly Nothing _ _ _ _) = return Nothing
tryUncons l@(Doubly (Just i0) _ _ _ _) = do a <- read l i0
                                            l' <- delete l i0
                                            return $ Just (a,l')
{-# INLINABLE tryUncons #-}

-- | Try to remove the last element from the list
tryUnsnoc :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Maybe (Doubly v (PrimState m) a, a))
tryUnsnoc (Doubly Nothing _ _ _ _) = return Nothing
tryUnsnoc l@(Doubly (Just i0) _ _ _ _) = do
  i <- unsafePrev l i0
  a <- read l i
  l' <- delete l i
  return $ Just (l',a)
{-# INLINABLE tryUnsnoc #-}

-- | O(1) Shift the list left by 1, placing the first element last
shiftL :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Doubly v (PrimState m) a)
shiftL l@(Doubly Nothing _ _ _ _) = return l
shiftL (Doubly (Just i0) free elems prevs nexts) = do
  i1 <- MV.read nexts i0
  return $ Doubly (Just i1) free elems prevs nexts

-- | O(1) Shift the list right by 1, placing the last element first
shiftR :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> m (Doubly v (PrimState m) a)
shiftR l@(Doubly Nothing _ _ _ _) = return l
shiftR (Doubly (Just i0) free elems prevs nexts) = do
  i_n <- MV.read prevs i0
  return $ Doubly (Just i_n) free elems prevs nexts

---------------
-- STREAMING --
---------------

stream :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Stream (Of a) m ()
stream (Doubly Nothing _ _ _ _) = return () -- empty
stream l@(Doubly (Just i0) _ _ _ _) = streamFrom l i0

streamWithKey :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Stream (Of (Index,a)) m ()
streamWithKey l = S.mapM (\i -> (i,) <$> read l i) $ streamKeys l

streamWithKeyFrom :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> Stream (Of (Index,a)) m ()
streamWithKeyFrom l = S.mapM (\i -> (i,) <$> read l i) . streamKeysFrom l

streamKeys :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Stream (Of Index) m ()
streamKeys (Doubly Nothing _ _ _ _) = return () -- empty
streamKeys l@(Doubly (Just i0) _ _ _ _) = streamKeysFrom l i0

streamFrom :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> Stream (Of a) m ()
streamFrom l@(Doubly _ _ elems _ _) = S.mapM (MV.read elems)
                                      . streamKeysFrom l

-- | Stream the indexes forward from a given starting index
streamKeysFrom :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> Stream (Of Index) m ()
streamKeysFrom (Doubly Nothing _ _ _ _) = emptyErr "streamKeysFrom"
streamKeysFrom (Doubly (Just i0) _ _ _ nexts) = go
  where go i = do S.yield i
                  nxt <- lift $ MV.read nexts i
                  when (nxt /= i0) $ go nxt -- cont.

-- | Read the doubly-linked list into a singly-linked list in reverse
-- order. Use toRevStream to not @sequence@ the reads.
toRevList :: (PrimMonad m, MVector v a) => Doubly v (PrimState m) a -> m [a]
toRevList = S.toList_ . revStream

revStream :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Stream (Of a) m ()
revStream (Doubly Nothing _ _ _ _) = return () -- empty
revStream l@(Doubly (Just i0) _ _ prevs _) = lift (MV.read prevs i0)
                                             >>= revStreamFrom l

revStreamFrom :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> Stream (Of a) m ()
revStreamFrom l@(Doubly _ _ elems _ _) = S.mapM (MV.read elems)
                                         . revStreamKeysFrom l

revStreamKeys :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Stream (Of Index) m ()
revStreamKeys (Doubly Nothing _ _ _ _) = return () -- empty
revStreamKeys l@(Doubly (Just i0) _ _ prevs _) = lift (MV.read prevs i0)
                                                 >>= revStreamKeysFrom l

-- | Stream the indexes forward from a given starting index
revStreamKeysFrom :: (PrimMonad m, MVector v a) =>
  Doubly v (PrimState m) a -> Index -> Stream (Of Index) m ()
revStreamKeysFrom (Doubly Nothing _ _ _ _) = emptyErr "revStreamKeysFrom"
revStreamKeysFrom (Doubly (Just i0) _ _ prevs _) = go
  where go i = do S.yield i
                  prv <- lift $ MV.read prevs i
                  if prv == i0 then S.yield i0 else go prv -- cont.

-----------
-- ERROR --
-----------

err :: String -> String -> a
err fun msg = error $ "Doubly." ++ fun ++ ": " ++ msg

emptyErr :: String -> a
emptyErr = flip err "empty list"

oobErr :: String -> (Int,Int) -> a
oobErr fun bs = err fun $ "out of bounds: " ++ show bs

undefErr :: String -> Int -> a
undefErr fun i = err fun $ "index " ++ show i ++ " marked undefined"
