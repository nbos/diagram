{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrIntervals (module Diagram.ConstrIntervals) where

import Control.Monad hiding (join)
import Control.Lens hiding (Index,(:>))
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.Strict.Tuple (Pair((:!:)),(:!:))
import qualified Data.Strict.Tuple as Pair

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Streaming hiding (join)
import qualified Streaming.Prelude as S

import Diagram.Primitive
import Diagram.String
import qualified Diagram.Doubly as D
import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import qualified Diagram.UnionType as UT

import Diagram.Util

data CI = CI { _headIndex  :: !Index
             , _headSymbol :: !Sym
             , _ciLength   :: !Len
             , _tailIndex  :: !Index
             , _tailSymbol :: !Sym }
  deriving(Show,Eq)

-- | Given the reference string and a contructive interval, produce the
-- list of indexed symbols that form the interval, starting at the head
-- and ending at the tail.
extension :: PrimMonad m => Doubly (PrimState m) -> CI -> m [(Index,Sym)]
extension _ (CI hd shd 2 tl stl) = return [(hd,shd),(tl,stl)]
extension str (CI hd shd len _ _)
  | len < 3 = error $ "CI.extension: invalid length: " ++ show len
  | otherwise = fmap ((hd,shd):) $
                S.toList_ . S.take (len-1) . D.streamWithKeyFrom str
                =<< D.unsafeNext str hd

-- | All construction sites, as intervals, for fast join\/union but no
-- delete\/subtract.
data CIs = CIs
  { _jointType :: !JointType
  , _symCounts :: !(IntMap Count) -- ::  s --> n
  , _byHead    :: !(IntMap CI)    -- :: hd --> (hd, shd, len, tl, stl)
  , _byTail    :: !(IntMap CI) }  -- :: tl --> (hd, shd, len, tl, stl)
  deriving(Show,Eq)

makeLenses ''CIs
makeLenses ''CI

instance Semigroup CIs where
  (<>) :: CIs -> CIs -> CIs
  (<>) = join

instance Monoid CIs where
  mempty :: CIs
  mempty = empty

------------------
-- CONSTRUCTION --
------------------

empty :: CIs
empty = CIs JT.bot e e e
  where e = IM.empty

singleton :: (Index,Sym) -> (Index,Sym) -> CIs
singleton (i0,s0) (i1,s1) = CIs jt ns bhd btl
  where jt = JT.singleton s0 s1
        ns = IM.fromList [(s0,1),(s1,1)]
        ci = CI i0 s0 2 i1 s1
        bhd = IM.singleton i0 ci
        btl = IM.singleton i1 ci

fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Map (Sym,Sym) CIs, r)
fromStream = flip fromStream_ M.empty

fromStream_ :: Monad m =>
  Stream (Of (Index,Sym)) m r -> Map (Sym,Sym) CIs -> m (Map (Sym,Sym) CIs, r)
fromStream_ ss m = (S.next ss >>=) $ \case
  Left r -> return (m, r)
  Right (is,ss') -> fromStream_0 is ss' m

fromStream_0 :: Monad m => (Index,Sym) ->
  Stream (Of (Index,Sym)) m r -> Map (Sym,Sym) CIs -> m (Map (Sym,Sym) CIs, r)
fromStream_0 is0@(i0,s0) ss !m = (S.next ss >>=) $ \case
  Left r -> return (m, r)
  Right (is1@(i1,s1),ss')
    | s0 /= s1 -> fromStream_0 (i1,s1) ss' $
      let ci = CI i0 s0 2 i1 s1
      in flip3 M.insertWith (s0,s1) (singleton is0 is1) m $ \_ ->
        (symCounts %~ IM.insertWith (+) s0 1 . IM.insertWith (+) s1 1)
        . (byHead %~ IM.insertWithKey err i0 ci)
        . (byTail %~ IM.insertWithKey err i1 ci)

    | otherwise -> do -- s0 == s1
        is :> ss'' <- S.toList $ S.map fst $ S.span ((s0 ==) . snd) ss'
        let len = length is + 2
            itl = last $ i1:is
            constrlen = (len `div` 2) * 2
            empty' = empty{ _jointType = JT.singleton s0 s0 }
            ci = CI i0 s0 len itl s0

        fromStream_0 (itl,s0) ss'' $ m & at (s0,s0) . non empty' %~
          (symCounts %~ IM.insertWith (+) s0 constrlen)
          . (byHead %~ IM.insertWithKey err i0 ci)
          . (byTail %~ IM.insertWithKey err itl ci)

  where
    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("ConstrIntervals.fromStream: collision: " ++) . show .:. (,,)

----------------
-- CONVERSION --
----------------

-- | Produce a list of heads + len determining the set of intervals
toList :: CIs -> [CI]
toList = IM.elems . _byHead

-- | Intervals are great for join-ing but differences (subtracting
-- sites) and tracking delta delta deleta (d3) counts requires a more
-- explicit representation. Sites contain all constructive indexes (s0's
-- specifically) of a set of joints.
type Sites = IntSet

-- | Given the reference string (read only), convert the
-- constr. intervals to the explicit set of constructive indexes.
toSites :: PrimMonad m => Doubly (PrimState m) -> CIs -> m Sites
toSites str cis = fmap (IS.fromList . concat) $
  forM (toList cis) $ \(CI hd _ len _ _) ->
    if len < 4 then return [hd]
    else fmap everyOther $ S.toList_ $
         S.take len $ D.streamKeysFrom str hd
  where
    everyOther [] = []
    everyOther [a] = [a]
    everyOther (a:_:rest) = a:everyOther rest

-----------------
-- COMPOSITION --
-----------------

-- | "Union" or "join" of two sets of constructive intervals which are
-- assumed to be disjoint in their joint types (implying also disjoint
-- in the constructive sites/intervals). Under this assumption,
-- collisions only have to be detected on the ends (head and tail) of
-- each interval without considering the symbols in between.
join :: CIs -> CIs -> CIs
join = fst .: join_

-- | More general join function which leaks the difference in counts
-- between the sum of the counts of each set of intervals and the counts
-- of the returned set of intervals (snd)
join_ :: CIs -> CIs -> (CIs, IntMap Int)
join_ ciAs ciBs = runIdentity $ flip evalStateT (ciAs :!: ciBs) $ do
  dns <- if uB0 `UT.disjoint` uA1 then return IM.empty else
    uses2 (_B.byHead) (_A.byTail) IM.intersection
    >>= go IM.empty . IM.elems

  dns' <- if uA0 `UT.disjoint` uB1 then return dns else do
    cols <- uses2 (_A.byHead) (_B.byTail) IM.intersection
    modify Pair.swap -- A <--> B
    res <- go dns $ IM.elems cols
    modify Pair.swap -- B <--> A
    return res

  let ns' = flip2 L.foldl' ns (IM.toList dns') $ \im (s,dn) ->
        flip2 IM.alter s im $ \case
        Nothing -> Just dn
        Just n -> nothingIf (==0) $ n + dn

  bhds <- uses2 (_A.byHead) (_B.byHead) $ IM.unionWithKey err
  btls <- uses2 (_A.byTail) (_B.byTail) $ IM.unionWithKey err

  return (CIs jt ns' bhds btls, dns')

  where
    JT uA0 uA1 = ciAs^.jointType
    JT uB0 uB1 = ciBs^.jointType
    jt = JT.join (ciAs^.jointType) (ciBs^.jointType)
    ns = IM.unionWith (+) (ciAs^.symCounts) (ciBs^.symCounts)

    _A :: forall s t a b. Field1 s t a b => Lens s t a b
    _B :: forall s t a b. Field2 s t a b => Lens s t a b
    _A = _1
    _B = _2

    go :: IntMap Int -> [CI] ->
          StateT (CIs :!: CIs) Identity (IntMap Int)
    go dns [] = return dns
    go dns ((CI tlA@hdB _ lenB tlB stlB):rest) = do
      _A.byTail %= IM.delete tlA -- delete tlA
      _B.byHead %= IM.delete hdB -- delete hdB (1/2)
      _B.byTail %= IM.delete tlB -- delete tlB (2/2)

      CI hdA shdA lenA _ _ <- (_A.byTail) `uses` (IM.! tlA)
      dns' <- ((_A.byHead) %%= deleteLookup tlB >>=) $ \case

        -- simple collision: [hdA..tlA) <> [hdB..tlB] ==> [hdA..tlB]
        Nothing -> do
          let lenA' = lenA + lenB - 1
              ci = CI hdA shdA lenA' tlB stlB
          _A.byHead %= IM.insert hdA ci -- update hdA
          _A.byTail %= IM.insert tlB ci -- insert tlB

          let oldInc | even lenB = 1
                     | otherwise = 0
              newInc | even lenA' = 1
                     | otherwise = 0
              delta = newInc - oldInc

          return $ if delta == 0 then dns -- (no change)
            else flip2 IM.alter stlB dns $ \case
            Nothing -> Just delta
            Just stln -> nothingIf (== 0) (stln + delta)

        -- double: [hdA..tlA) <> [hdB..tlB) <> [hdA2..tlA2] ==> [hdA..tlA2]
        Just (CI _ _ lenA2 tlA2 stlA2) -> do
          let hdA2 = tlB -- aliases
              lenA' = lenA + lenB + lenA2 - 2
              ci = CI hdA shdA lenA' tlA2 stlA2
          _A.byHead %= IM.insert hdA ci  -- update hdA
          _A.byHead %= IM.delete hdA2    -- delete hdA2
          _A.byTail %= IM.insert tlA2 ci -- update tlA2

          let oldIncA2 | even lenA2 = 1
                       | otherwise = 0
              newIncA2 | even lenA' = 1
                       | otherwise = 0
              delta = newIncA2 - oldIncA2

          return $
            (if even lenB then id else flip IM.alter stlB $ \case
                Nothing -> Just 1
                Just stln -> nothingIf (== 0) (stln + 1)) $
            (if delta == 0 then id else flip IM.alter stlA2 $ \case
                Nothing -> Just delta -- assert (delta == 1)
                Just stln -> nothingIf (== 0) (stln + delta)) dns

      go dns' rest -- continue

    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("ConstrIntervals.join: collision: " ++) . show .:. (,,)


-- | Use the target of two lenses in the current state with a function
uses2 :: MonadState s m => Lens' s a -> Lens' s b -> (a -> b -> c) -> m c
uses2 a b = uses a >=> uses b
{-# INLINE uses2 #-}

-- | Use the target of three lenses in the current state with a function
uses3 :: MonadState s m =>
  Lens' s a -> Lens' s b -> Lens' s c -> (a -> b -> c -> d) -> m d
uses3 a b c = uses a >=> uses b >=> uses c
{-# INLINE uses3 #-}

deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)
{-# INLINE deleteLookup #-}
