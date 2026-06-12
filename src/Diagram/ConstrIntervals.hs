{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrIntervals (module Diagram.ConstrIntervals) where

import Control.Monad hiding (join)
import Control.Lens hiding (Index,(:>))
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Streaming (Of(..), Stream)
import qualified Streaming.Prelude as S

import Diagram.Primitive (PrimMonad(..))
import Diagram.String (Index, Count, Doubly, Sym)
import qualified Diagram.Doubly as D
import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import qualified Diagram.UnionType as UT

import Diagram.ConstrInterval ( CI(..)
                              , headIndex --, headSymbol
                              , tailIndex, tailSymbol )
import qualified Diagram.ConstrInterval as CI

import Diagram.Util

-- | All construction sites, as intervals, for fast join\/union but no
-- delete\/subtract.
data CIs = CIs
  { _jointType :: JointType    -- :: (u0, u1)
  , _symCounts :: IntMap Count -- :: s  --> n
  , _byHead    :: IntMap CI    -- :: hd --> (hd, shd, len, tl, stl)
  , _byTail    :: IntMap CI }  -- :: tl --> (hd, shd, len, tl, stl)
  deriving(Show,Eq)

makeLenses ''CIs

------------------
-- CONSTRUCTION --
------------------

-- | Empty set of construction intervals
empty :: CIs
empty = CIs JT.bot e e e
  where e = IM.empty

-- | A singleton set containing a singleton interval
singleton :: (Index,Sym) -> (Index,Sym) -> CIs
singleton is0@(i0,s0) is1@(i1,s1) = CIs jt ns bhd btl
  where jt = JT.singleton s0 s1
        ns = IM.fromList [(s0,1),(s1,1)]
        ci = CI.singleton is0 is1
        bhd = IM.singleton i0 ci
        btl = IM.singleton i1 ci

-- | Construction, for every pair of symbols in the given source string,
-- of the set of continuous construction intervals.
fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Map (Sym,Sym) CIs, r)
fromStream = flip fromStream_ M.empty

-- | Construction given a source string and an accumulator map keyed on
-- joints.
fromStream_ :: Monad m =>
  Stream (Of (Index,Sym)) m r -> Map (Sym,Sym) CIs -> m (Map (Sym,Sym) CIs, r)
fromStream_ ss m = (S.next ss >>=) $ \case
  Left r -> return (m, r)
  Right (is,ss') -> fromStream_0 is ss' m

-- | Construction given a head symbol, the rest of the source string and
-- an accumulator map keyed on joints.
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

toList :: CIs -> [CI]
toList = IM.elems . _byHead

-- | Intervals are great for join-ing but differences (subtracting
-- sites) and tracking delta delta deleta (d3) counts requires a more
-- explicit representation. Sites contain all constructive indexes (s0's
-- specifically) of a set of joints.
type Sites = IntSet

-- | (TODO: DELETE (OLD LOGIC)) Given the reference string (read only),
-- convert the constr. intervals to the explicit set of constructive
-- indexes.
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

data JoinState = JoinState
  { __A :: !CIs -- some set of intervals 'A'
  , __B :: !CIs -- some other set of intervals 'B'
  , _delta :: !(IntMap Int) -- sym count delta accumulator
} deriving (Show,Eq)
makeLenses ''JoinState

instance Semigroup CIs where
  (<>) :: CIs -> CIs -> CIs
  (<>) = join

instance Monoid CIs where
  mempty :: CIs
  mempty = empty

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
join_ ciAs ciBs = runIdentity $ flip evalStateT (JoinState ciAs ciBs IM.empty) $ do

  unless (uB0 `UT.disjoint` uA1) $ -- short-circuit intersection (O(m) <<< O(N))
    uses2 (_B.byHead) (_A.byTail) IM.intersection
    >>= mapM_ go

  unless (uA0 `UT.disjoint` uB1) $ do -- short-circuit intersection (O(m) <<< O(N))
    cols <- uses2 (_A.byHead) (_B.byTail) IM.intersection
    modify $ \(JoinState a b d) -> JoinState b a d -- A <--> B
    mapM_ go cols
    modify $ \(JoinState b a d) -> JoinState a b d -- B <--> A

  -- fold new sym count deltas into the counts map
  dns <- use delta
  bhds <- uses2 (_A.byHead) (_B.byHead) $ IM.unionWithKey err
  btls <- uses2 (_A.byTail) (_B.byTail) $ IM.unionWithKey err
  let ns' = L.foldl' (flip $ uc alter) ns (IM.toList dns)

  return (CIs jt ns' bhds btls, dns)

  where
    JT uA0 uA1 = ciAs^.jointType
    JT uB0 uB1 = ciBs^.jointType
    jt = JT.join (ciAs^.jointType) (ciBs^.jointType)
    ns = IM.unionWith (+) (ciAs^.symCounts) (ciBs^.symCounts)

    alter s d = flip IM.alter s $ \case
      Nothing -> Just d
      Just n -> nothingIf (== 0) (n + d)

    inc = inc_ 1
    -- dec = inc_ (-1)
    inc_ :: Int -> Sym -> StateT JoinState Identity ()
    inc_ d s = delta %= alter s d

    -- | Given a constructive interval from 'B' whose head collides with
    -- the tail of an interval of the other set 'A', join together, fix
    -- count if required and re-insert in 'A' set
    go :: CI -> StateT JoinState Identity ()
    go ciB@(CI tlA@hdB _ _ tlB _) = do
      _A.byTail %= IM.delete tlA -- delete [.. tlA]
      _B.byHead %= IM.delete hdB -- delete [hdB ..]
      _B.byTail %= IM.delete tlB -- delete [.. tlB]

      -- can't accept a ciA independent of any previous go calls because
      -- it could have been prepended to (sandwich case), so we lookup
      ciA <- (_A.byTail) `uses` (IM.! tlA)
      when (CI.odd ciA) $ inc (ciA^.tailSymbol)

      let ciAB = CI.unsafeJoin ciA ciB
      ((_A.byHead) %%= deleteLookup tlB >>=) $ \case
        -- simple collision: [hdA..tlA) <> [hdB..tlB] ==> [hdA..tlB]
        Nothing -> do
          _A.byHead %= IM.insert (ciAB^.headIndex) ciAB -- update hdA
          _A.byTail %= IM.insert (ciAB^.tailIndex) ciAB -- insert tlB
          let d = fromEnum (CI.even ciAB) - fromEnum (CI.even ciB)
          unless (d == 0) $ inc_ d (ciAB^.tailSymbol)

        -- sandwich: [hdA..tlA) <> [hdB..tlB) <> [hdA2..tlA2] ==> [hdA..tlA2]
        Just ciA2 -> do
          when (CI.odd ciB) $ inc (ciA2^.tailSymbol)
          let ciABA = CI.unsafeJoin ciAB ciA2
          _A.byHead %= IM.delete (ciA2^.headIndex) -- delete hdA2
          _A.byHead %= IM.insert (ciABA^.headIndex) ciABA -- update hdA
          _A.byTail %= IM.insert (ciABA^.tailIndex) ciABA -- update tlA2
          let d = fromEnum (CI.even ciABA) - fromEnum (CI.even ciA2)
          unless (d == 0) $ inc_ d (ciABA^.tailSymbol)

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
