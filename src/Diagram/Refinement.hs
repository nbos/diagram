{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TupleSections, LambdaCase, TypeApplications, TypeOperators, BangPatterns #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.Refinement (module Diagram.Refinement) where

import Control.Monad as Monad
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict ( StateT
                                  , MonadState(get)
                                  , evalStateT )
import Control.Monad.Random (MonadRandom(getRandomR, getRandom))

import Data.Maybe
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as U

import Streaming hiding (first,second)
import qualified Streaming.Prelude as S

import Diagram.Primitive
import Diagram.Information

import Diagram.String
import qualified Diagram.Doubly as D
import Diagram.Joints (Joints,Joints2(J2),Joints2S(J2S))
import qualified Diagram.Joints as Jts
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(..),left,right)
import qualified Diagram.JointType as JT

import Diagram.Util

err :: String -> a
err = error . ("Refinement." ++)

-- ----------------------- --
-- -- RANDOM GENERATION -- --
-- ----------------------- --

-- | O(n^2) Generate all refinements given joints indexed both ways
-- starting with the empty refinement, ending with the same as input
--
-- >>> import qualified Diagram.Joints as Jts
-- >>> enumRefinements $ Jts.sized $ Jts.doubleIndex 256 $ Jts.fromList [1,2,3,2]
-- [ fromLists ([],[])
-- , fromLists ([3],[2])
-- , fromLists ([2],[3])
-- , fromLists ([2,3],[2,3])
-- , fromLists ([1],[2])
-- , fromLists ([1,3],[2])
-- , fromLists ([1,2],[2,3])
-- , fromLists ([1,2,3],[2,3]) ]
enum :: forall a. Joints2S a -> [JointType]
enum (J2S byFst0 bySnd0) = concatMap givenU0 u0s
  where -- (enumerate u0 powerset, then hitting set enum)
    u0s :: [[(Sym, Map Sym a)]]
    u0s = combs $ M.toAscList byFst0 -- deconstruct, select

    givenU0 :: [(Sym, Map Sym a)] -> [JointType]
    givenU0 s0ns0ss = JT u0 . UT.fromDistinctAscList
                      <$> go byFst (M.toAscList bySnd)
      where
        byFst = M.fromAscList s0ns0ss -- reconstruct
        bySnd = fmap (`M.intersection` byFst) $ -- restrict to s0s
                bySnd0 `M.intersection` s1s -- restrict to s1s
        (u0,s1s) = UT.fromDistinctAscList *** M.unions $ unzip s0ns0ss

    go :: Map Sym (Map Sym a) -> [(Sym, Map Sym a)] -> [[Sym]]
    go _ [] = [[]]
    go byFst ((s1,s0s):bySnd)
      | malformed = error "enumRefinements: malformed"
      | notFree = sel
      | otherwise = notSel ++ sel
      where
        ns1s = byFst `M.intersection` s0s
        malformed = any (s1 `M.notMember`) ns1s
        notFree = any ((== 1) . M.size) ns1s
        sel = (s1:) <$> go byFst bySnd -- leave s1 in

        -- remove s1 if not selected
        ns1s' = M.delete s1 <$> ns1s
        byFst' = ns1s' `M.union` byFst
        notSel = go byFst' bySnd

-- | O(n^2) Generate all combinations
combs :: [a] -> [[a]]
combs [] = [[]]
combs (a:as) = ass ++ fmap (a:) ass
  where ass = combs as

-- | State record to track the two maps and two sets
data GenerationState a = GenerationState {
  -- NOTE: Map Int instead of IntMap because we want O(1) size
  -- and O(log n) elemAt.
  _jtsByFst :: !(Map Sym ([(Sym,a)], Map Sym a)),
  _jtsBySnd :: !(Map Sym ([(Sym,a)], Map Sym a)),
  _fstUnion :: !IntSet,
  _sndUnion :: !IntSet,
  _refJoints :: !(Map (Sym,Sym) a)
}
makeLenses ''GenerationState

-- | Generate a random refinement, given a set of joints indexed both
-- ways. Assumes each map maps pairs of symbols to the same values,
-- i.e. if s0 -> s1 -> a01 then s1 -> s0 -> a01, otherwise the returned
-- map of joints will have unpredicatble values.
genRandom :: (MonadRandom m, PrimMonad m) =>
             Joints2S a -> m (JointType, Joints a)
genRandom = genRandomWith 0.5

-- | Generate a random refinement, given a sampling probability
genRandomWith :: forall m a. (MonadRandom m, PrimMonad m) =>
                 Double -> Joints2S a -> m (JointType, Joints a)
genRandomWith r (J2S byFst0 bySnd0) =
  evalStateT go $ GenerationState
  (([],) <$> byFst0)
  (([],) <$> bySnd0) IS.empty IS.empty M.empty
  where
    go :: StateT (GenerationState a) m (JointType, Map (Sym,Sym) a)
    go = get >>= \case
      (GenerationState byFst bySnd u0 u1 ref)
        | remaining == 0 -> return ( JT (UT.fromSet u0) (UT.fromSet u1)
                                   , ref ) -- end
        | otherwise -> do
            i <- getRandomR (0, remaining-1) -- select a symbol
            f <- getRandom @_ @Double -- include/exclude it in the ref
            let b = f <= r
            if i < len0 then goElimFst b (fst $ M.elemAt i byFst)
              else goElimSnd b (fst $ M.elemAt (i - len0) bySnd)
            go -- rec
        where
          len0 = M.size byFst -- O(1)
          len1 = M.size bySnd -- O(1)
          remaining = len0 + len1

    -- | Map.deleteFind
    deleteFind :: Ord k => k -> Map k b -> (b, Map k b)
    deleteFind = first fromJust
                 .: M.updateLookupWithKey (\_ _ -> Nothing)

    -- | Eliminate a symbol and enforce invariants whether it has be
    -- `sel`ected or not
    goElimFst :: Bool -> Int -> StateT (GenerationState a) m ()
    goElimFst sel0 s0 = do
      (staged0, jt0s) <- jtsByFst %%= deleteFind s0 -- remove from avail.
      when sel0 $ do
        fstUnion %= IS.insert s0 -- add to JointType
        forM_ staged0 $ \(s1,a01) -> -- add staged Joints
          refJoints %= M.insert (s0,s1) a01

      -- unlink from neighbors
      deleted1 <- forM (M.keys jt0s) $ \s1 -> do
        jtsBySnd . at s1 %%= \case
          Nothing -> error "impossible"
          Just (staged1, jt1s) -- -> (deleted, inserted)
            | null staged1' && M.null jt1s' -> (Just s1, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged1', jt1s')) -- update
            where
              staged1' | sel0 = (s0,a01):staged1 -- stage s0 on s1 if selected
                       | otherwise = staged1
              (a01,jt1s') = deleteFind s0 jt1s

      -- enforce invariant if necessary
      when (sel0 && null staged0) $ do
        i <- getRandomR (0, M.size jt0s - 1) -- select a symbol
        let (s1, a01) = M.elemAt i jt0s
        if Just s1 `notElem` deleted1 then goElimSnd True s1 -- rec
          else do sndUnion %= IS.insert s1
                  refJoints %= M.insert (s0,s1) a01 -- null staged1

    -- | Symmetric with above, could probably be factored into one, but
    -- ehhh
    goElimSnd :: Bool -> Int -> StateT (GenerationState a) m ()
    goElimSnd sel1 s1 = do
      (staged1, jt1s) <- jtsBySnd %%= deleteFind s1 -- remove from avail.
      when sel1 $ do
        sndUnion %= IS.insert s1 -- add to JointType
        forM_ staged1 $ \(s0,a01) -> -- add staged Joints
          refJoints %= M.insert (s0,s1) a01

      -- unlink from neighbors
      deleted0 <- forM (M.keys jt1s) $ \s0 -> do
        jtsByFst . at s0 %%= \case
          Nothing -> error "impossible"
          Just (staged0, jt0s) -- -> (deleted, inserted)
            | null staged0' && M.null jt0s' -> (Just s0, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged0', jt0s')) -- update
            where
              staged0' | sel1 = (s1,a01):staged0 -- stage s1 on s0 if selected
                       | otherwise = staged0
              (a01,jt0s') = deleteFind s1 jt0s

      -- enforce invariant if necessary
      when (sel1 && null staged1) $ do
        i <- getRandomR (0, M.size jt1s - 1) -- select a symbol
        let (s0, a01) = M.elemAt i jt1s
        if Just s0 `notElem` deleted0 then goElimFst True s0 -- rec
          else do fstUnion %= IS.insert s0
                  refJoints %= M.insert (s0,s1) a01 -- null staged0

-----------
-- SITES --
-----------

data IxdJoint = IJ !Index !Sym -- i0 s0
                !Index !Sym -- i1 s2

-- runsByHead :: PrimMonad m => JointType -> Doubly (PrimState m) ->
--               m (IntMap (NonEmpty (Sym,Sym)))
-- runsByHead jt ss = IM.fromDistinctAscList
--                    <$> S.toList_ (streamRuns jt ss)

streamRuns :: PrimMonad m => JointType -> Doubly (PrimState m) ->
              Stream (Of (NonEmpty IxdJoint)) m ()
streamRuns jt = streamRuns_ jt . D.streamWithKey

streamRuns_ :: Monad m => JointType -> Stream (Of (Index,Sym)) m r ->
               Stream (Of (NonEmpty IxdJoint)) m r
streamRuns_ (JT u0 u1) = go0
  where
    go0 iss = (lift (S.next iss) >>=) $ \case
      Left r -> return r -- end
      Right ((i0,s0),iss')
        | s0 `UT.member` u0 -> go1 i0 s0 iss'
        | otherwise -> go0 iss'

    go1 i0 s0 iss = (lift (S.next iss) >>=) $ \case
      Left r -> return r -- end
      Right ((i1,s1),iss')
        | s1 `UT.member` u1 -> do (tl, cont) <- lift $ go2 iss'
                                  S.yield (IJ i0 s0 i1 s1 :| tl)
                                  cont
        | s1 `UT.member` u0 -> go1 i1 s1 iss'
        | otherwise -> go0 iss'

    go2 iss = (S.next iss >>=) $ \case
      Left r -> return ([], return r)
      Right ((i0,s0),iss')
        | s0 `UT.notMember` u0 -> return ([], go0 iss')
        | otherwise -> (S.next iss' >>=) $ \case
            Left r -> return ([], return r)
            Right ((i1,s1), iss'')
              | s1 `UT.member` u1 -> first (IJ i0 s0 i1 s1:) <$> go2 iss''
              | s1 `UT.member` u0 -> return ([], go1 i1 s1 iss')
              | otherwise -> return ([], go0 iss'')

deltaByMut :: forall m r. PrimMonad m => JointType -> Doubly (PrimState m) ->
              Stream (Of (NonEmpty IxdJoint)) m r ->
              Stream (Of (SomeMutation, IntMap Int)) m r
deltaByMut jt ss ijts = (lift (D.lastKey ss) >>=) $ \case
  Nothing -> lift $ S.effects ijts -- ijts should be empty so drain
  Just iLast -> deltaByMut_ jt ss iLast ijts

deltaByMut_ :: forall m r. PrimMonad m => JointType -> Doubly (PrimState m) -> Index ->
               Stream (Of (NonEmpty IxdJoint)) m r ->
               Stream (Of (SomeMutation, IntMap Int)) m r
deltaByMut_ (JT u0 u1) ss iLast = go
  where
    go :: Stream (Of (NonEmpty IxdJoint)) m r ->
          Stream (Of (SomeMutation, IntMap Int)) m r
    go ijts = (lift (S.next ijts) >>=) $ \case
      Left r -> return r -- end
      Right (IJ i0 s0 i1 s1 :| rest, ijts') -> do
        im1 <- prev i0
        if im1 == iLast then go ijts' -- skip
          else do
          sm1 <- read im1
          let inLeft = sm1 `UT.member` u0
              inRight = s0 `UT.member` u1

              mut | inLeft = Mut (AddRight s0) -- assert (not inRight)
                  | inRight = Mut (AddLeft sm1) -- assert (not inLeft)
                  | otherwise = Mut (Add2 sm1 s0)

              u0' | inLeft = u0
                  | otherwise = UT.insertMissing sm1 u0

              u1' | inRight = u1
                  | otherwise = UT.insertMissing s0 u1

          undefined


    prev :: MonadTrans t => Index -> t m Index
    prev = lift . D.unsafePrev ss

    next :: MonadTrans t => Index -> t m Index
    next = lift . D.unsafeNext ss

    read :: MonadTrans t => Index -> t m Sym
    read = lift . D.read ss

-- brokenSites :: forall m r. PrimMonad m => Doubly (PrimState m) ->
--                Stream (Of (Index, NonEmpty (Sym,Sym))) m r -> m (Of (Joints Sites) r)
-- brokenSites = S.fold (flip insert) M.empty id .: brokenSites_
--   where insert (s0s1,i) = M.insertWith (const $ (+1) `bimap` IS.insert i)
--                           s0s1 (1 :!: IS.singleton i)

-- -- | Given a reference string and a stream of construction runs, enumerate the
-- brokenSites_ :: forall m r. PrimMonad m => Doubly (PrimState m) ->
--                 Stream (Of (Index, NonEmpty (Sym,Sym))) m r ->
--                 Stream (Of ((Sym,Sym), Index)) m r
-- brokenSites_ ss irs0 = (lift (D.lastKey ss) >>=) $ \case
--   Nothing -> error "Refinement.brokenSites: empty string"
--   Just iLast -> go iLast irs0
--   where
--     next :: MonadTrans t => Index -> t m Index
--     next = lift . D.nextKey ss

--     go iLast = goStr where
--       goStr :: Stream (Of (Index, NonEmpty (Sym,Sym))) m r ->
--                Stream (Of ((Sym,Sym), Index)) m r
--       goStr irs = (lift (S.next irs) >>=) $ \case
--         Left r -> return r
--         Right ((i0,(_,s1):|rest), irs') ->
--           lift (D.nextKey ss i0) -- \i1 ->
--           >>= flip2 goRun s1 rest
--           >> goStr irs'

--       goRun :: Int -> Sym -> [(Sym,Sym)] -> Stream (Of ((Sym,Sym), Int)) m ()
--       goRun i1 s1 ((s2,s3):rest) =
--         S.yield ((s1,s2),i1)
--         >> (next i1 >>= next) -- \i3 ->
--         >>= flip2 goRun s3 rest

--       goRun i1 s1 []
--         | i1 == iLast = return () -- end of string, nothing broken
--         | otherwise = do s2 <- lift . D.read ss =<< next i1
--                          S.yield ((s1,s2),i1)

-- -------------- --
-- -- MUTATION -- --
-- -------------- --

-- data Mutation =
--   AddLeft  !Sym !(IntMap Int)
--   | AddRight !Sym !(IntMap Int)
--   | Add2     !Sym !Sym !Int
--   | DelLeft  !Sym !(IntMap Int)
--   | DelRight !Sym !(IntMap Int)
--   | Del2     !Sym !Sym !Int
--   deriving(Show,Eq)

data SidesAffected = One | Two
  deriving (Show,Eq,Ord)

type family NewJoints (k :: SidesAffected) where
  NewJoints One = IntMap Int -- ins opposite side
  NewJoints Two = Int -- n01

data Mutation (k :: SidesAffected) where
  AddLeft  :: !Sym         -> Mutation One
  AddRight :: !Sym         -> Mutation One
  Add2     :: !Sym -> !Sym -> Mutation Two
  DelLeft  :: !Sym         -> Mutation One
  DelRight :: !Sym         -> Mutation One
  Del2     :: !Sym -> !Sym -> Mutation Two
-- TODO: I don't think we need GADT anymore since
-- MutationDelta always takes an IntMap of counts

deriving instance Show (Mutation k)
deriving instance Eq (Mutation k)
deriving instance Ord (Mutation k)

---------------------------------
-- SOME MUTATION (EXISTENTIAL) --
---------------------------------

data SomeMutation where
  Mut :: !(Mutation k) -> SomeMutation

instance Show SomeMutation where
  show :: SomeMutation -> String
  show (Mut mut) = case mut of
    AddLeft  s0    -> "AddLeft "  ++ show s0
    AddRight s1    -> "AddRight " ++ show s1
    Add2     s0 s1 -> "Add2 "     ++ show s0 ++ " " ++ show s1
    DelLeft  s0    -> "DelLeft "  ++ show s0
    DelRight s1    -> "DelRight " ++ show s1
    Del2     s0 s1 -> "Del2 "     ++ show s0 ++ " " ++ show s1

instance Eq SomeMutation where
  (==) :: SomeMutation -> SomeMutation -> Bool
  Mut mut == Mut mut' = case (mut, mut') of
    (AddLeft  s, AddLeft  s')  ->  s == s'
    (AddRight s, AddRight s')  ->  s == s'
    (Add2 s0 s1, Add2 s0' s1') -> s0 == s0' && s1 == s1'
    (DelLeft  s, DelLeft  s')  ->  s == s'
    (DelRight s, DelRight s')  ->  s == s'
    (Del2 s0 s1, Del2 s0' s1') -> s0 == s0' && s1 == s1'
    _else -> False

instance Ord SomeMutation where
  compare :: SomeMutation -> SomeMutation -> Ordering
  compare (Mut mut) (Mut mut') = case (mut, mut') of
    (AddLeft  s, AddLeft  s')  -> compare s s'
    (AddRight s, AddRight s')  -> compare s s'
    (Add2 s0 s1, Add2 s0' s1') -> compare s0 s0' <> compare s1 s1'
    (DelLeft  s, DelLeft s')   -> compare s s'
    (DelRight s, DelRight s')  -> compare s s'
    (Del2 s0 s1, Del2 s0' s1') -> compare s0 s0' <> compare s1 s1'
    _else -> compare (mutOrd mut) (mutOrd mut')
    where
      mutOrd :: Mutation k -> Int
      mutOrd AddLeft{}  = 0
      mutOrd AddRight{} = 1
      mutOrd Add2{}     = 2
      mutOrd DelLeft{}  = 3
      mutOrd DelRight{} = 4
      mutOrd Del2{}     = 5

-- -- | O(N log(m)) Reads the whole string compiling differences in counts
-- -- resulting from introduced mutations breaking old constructions
-- -- through higher precedence.
-- countDeltaByBreakingMut :: forall m. PrimMonad m =>
--   JointType -> Doubly (PrimState m) -> m (Map SomeMutation (IntMap Int))
-- countDeltaByBreakingMut jt str =
--   S.fold_ (flip $ uncurry $ M.insertWith (IM.unionWith (+))) M.empty id $
--   S.mapM mkEntry $ streamRuns jt str
--   where
--     mkEntry :: (Index, NonEmpty (Sym, Sym)) -> m (SomeMutation, IntMap Int)
--     mkEntry (i,ps) = do
--       s0 <- D.read str =<< D.prevKey str i
--       let (mut, _, s2) = breakingOf jt s0 ps
--           im = case compare s0 s2 of
--             LT -> IM.fromDistinctAscList [(s0,-1),(s2,1)]
--             EQ -> IM.empty -- cancel out
--             GT -> IM.fromDistinctAscList [(s2,1),(s0,-1)]
--       return (mut, im)

-- -- | For a joint type and a non-empty sequence of consecutive
-- -- construction sites (s1,s2):rest, preceded by a non-constructive
-- -- symbol s0, returns the mutation to that type that would break the
-- -- sequence, the length of the new run, and the trailing symbol that
-- -- would be left unconstructed, i.e. whose count would have to be
-- -- incremented by 1 (while the count of s0 would be correspondingly
-- -- decremented by 1).
-- breakingOf :: JointType -> Sym -> NonEmpty (Sym,Sym) -> (SomeMutation, Int, Sym)
-- breakingOf (JT u0 u1) s0 ((s1,s2):|rest) = uncurry (mut,,) $
--                                            go 1 s2 rest
--   where
--     go !k s2' [] = (k,s2')
--     go !k s2' ((s3,s4):rest')
--       | (s2',s3) `JT.member` jt' = go (k+1) s4 rest' -- breaking propagates
--       | otherwise = (k,s2') -- breaking stops, leaving s2' unconstructed

--     (mut, jt')
--       | s0 `UT.member` u0 = -- assert $ not (s1 `UT.member` u1)
--           (Mut (AddRight s1), JT u0 u1')
--       | s1 `UT.member` u1 = (Mut (AddLeft s0), JT u0' u1)
--       | otherwise = (Mut (Add2 s0 s1), JT u0' u1')
--       where
--         u0' = UT.insertMissing s0 u0
--         u1' = UT.insertMissing s1 u1

----------------------------
-- STATE & INITIALIZATION --
----------------------------

-- TODO: move to Diagram.Model
data ModelParams = Params
  { _numSymbols :: !Int -- m
  , _stringLength :: !Int -- N
  , _symbolCounts :: !(U.Vector Int) } -- ns
makeLenses ''ModelParams

-- Membership --

type MembershipT a = StateT (Membership a)
data Membership a = Membership -- :: {In,Out} <--> {In,Out}
  { _byFstInIn   :: !(IntMap (IntMap a))   --  In  --> In
  , _byFstInOut  :: !(IntMap (IntMap a))   --  In  --> Out
  , _byFstOutIn  :: !(IntMap (IntMap a))   -- Out  --> In
  , _byFstOutOut :: !(IntMap (IntMap a))   -- Out  --> Out
  , _bySndInIn   :: !(IntMap (IntMap a))   --  In <--  In
  , _bySndInOut  :: !(IntMap (IntMap a))   -- Out <--  In
  , _bySndOutIn  :: !(IntMap (IntMap a))   --  In <--  Out
  , _bySndOutOut :: !(IntMap (IntMap a)) } -- Out <--  Out
makeLenses ''Membership

initMembership :: Joints2 a -> JointType -> Membership a
initMembership (J2 byFst bySnd) (JT u0 u1) =
  Membership { _byFstInIn   = byFstInIn_
             , _byFstInOut  = byFstInOut_
             , _byFstOutIn  = byFstOutIn_
             , _byFstOutOut = byFstOutOut_
             , _bySndInIn   = bySndInIn_
             , _bySndInOut  = bySndInOut_
             , _bySndOutIn  = bySndOutIn_
             , _bySndOutOut = bySndOutOut_ }
  where
    byFstIn = byFst `IM.restrictKeys` UT.set u0
    byFstInIn_  = (`IM.restrictKeys` UT.set u1) <$> byFstIn
    byFstInOut_ = (`IM.withoutKeys`  UT.set u1) <$> byFstIn

    byFstOut = byFst `IM.withoutKeys` UT.set u0
    byFstOutIn_  = (`IM.restrictKeys` UT.set u1) <$> byFstOut
    byFstOutOut_ = (`IM.withoutKeys`  UT.set u1) <$> byFstOut

    bySndIn = bySnd `IM.restrictKeys` UT.set u1
    bySndInIn_  = (`IM.restrictKeys` UT.set u0) <$> bySndIn
    bySndInOut_ = (`IM.withoutKeys`  UT.set u0) <$> bySndIn

    bySndOut = bySnd `IM.withoutKeys` UT.set u1
    bySndOutIn_  = (`IM.restrictKeys` UT.set u0) <$> bySndOut
    bySndOutOut_ = (`IM.withoutKeys`  UT.set u0) <$> bySndOut

-- Refinement --

type RefinementT m = StateT (RefinementState (PrimState m)) m
data RefinementState s = RefinementState
  { _modelParams     :: !ModelParams  -- :: (m, N, ns)
  , _workingString   :: !(Doubly s)   -- :: ss (read only)
  -- , _parent          :: !JointType
  , _jointCount      :: !Int          -- :: nm
  , _refinement      :: !JointType    -- :: rjt
  , _newSymbolCounts :: !(IntMap Int) -- :: ns'
  , _membership      :: !(Membership Int) }
makeLenses ''RefinementState

initState :: ModelParams -> Doubly s -> Joints2 Int -> JointType -> RefinementState s
initState mdl@(Params _ _ ns) str jts2 rjt =
  RefinementState { _modelParams = mdl
                  , _workingString = str
                  , _jointCount = nm
                  , _refinement = rjt
                  , _newSymbolCounts = ns'
                  , _membership = coverage }
  where
    coverage = initMembership jts2 rjt
    nm = sum $ snd <$> byFstInInL
    byFstInInL = byFstToAscList $ coverage ^. byFstInIn
    ns' = IM.mapWithKey (\s dn -> (ns U.! s) - dn ) $
          IM.fromListWith (+) $
          foldr (\((s0,s1),n01) l -> (s0,n01):(s1,n01):l)
          [] byFstInInL

-------------------------
-- ENUMERATE MUTATIONS --
-------------------------

data MutationJoints where
  MutD :: !(Mutation k) -> !(NewJoints k) -> MutationJoints

instance Show MutationJoints where
  show :: MutationJoints -> String
  show (MutD mut ncjts) = case mut of
    AddLeft  s   -> "AddLeft "  ++ show s ++ " " ++ show ncjts
    AddRight s   -> "AddRight " ++ show s ++ " " ++ show ncjts
    Add2     a b -> "Add2 "     ++ show a ++ " " ++ show b ++ " " ++ show ncjts
    DelLeft  s   -> "DelLeft "  ++ show s ++ " " ++ show ncjts
    DelRight s   -> "DelRight " ++ show s ++ " " ++ show ncjts
    Del2     a b -> "Del2 "     ++ show a ++ " " ++ show b ++ " " ++ show ncjts

enumMutations :: Membership Int -> [MutationJoints]
enumMutations (Membership byFstInIn_ _ byFstOutIn_ byFstOutOut_
                bySndInIn_ _ bySndOutIn_ _) =
  als ++ ars ++ a2s ++ dls ++ drs ++ d2s
  where
    -- Out --> In
    als = uc (MutD . AddLeft)  <$> IM.toList byFstOutIn_
    -- In <-- Out
    ars = uc (MutD . AddRight) <$> IM.toList bySndOutIn_

    -- (Out,Out) that can't be added one-by-one
    a2s = uc (MutD . uc Add2) <$> M.toList outSubgraph
    outSubgraph = byFstToMap $
                  (IM.\\ bySndOutIn_)
                  <$> (byFstOutOut_ IM.\\ byFstOutIn_)

    leftDependents = IM.mapMaybe fromSingleton byFstInIn_
    rightDependents = IM.mapMaybe fromSingleton bySndInIn_
    fromSingleton im | [sn] <- IM.toList im = Just sn
                     | otherwise = Nothing

    -- can be deleted: In (w/o dependents) --> In
    dls = uc (MutD . DelLeft) <$> byFstInInWithoutDeps
    byFstInInWithoutDeps = filter (IM.disjoint rightDependents . snd) $
                           IM.toList byFstInIn_ -- In --> In

    -- can be deleted: In <-- In (w/o dependents)
    drs = uc (MutD . DelRight) <$> bySndInInWithoutDeps
    bySndInInWithoutDeps = filter (IM.disjoint leftDependents . snd) $
                           IM.toList bySndInIn_ -- In <-- In

    -- (In,In) that can't be deleted one-by-one
    d2s = uc (MutD . uc Del2) <$> M.toList coDependents
    coDependents = M.intersectionWith assertEq
                   leftPendantJts rightPendantJts

    leftPendantJts = M.fromDistinctAscList $ toList leftDependents
      where toList :: IntMap (s1, a) -> [((Sym, s1), a)]
            toList = IM.foldrWithKey (\s0 s1n l -> first (s0,) s1n : l) []

    rightPendantJts = M.fromDistinctAscList $ toList rightDependents
      where toList :: IntMap (s0, a) -> [((s0, Sym), a)]
            toList = IM.foldrWithKey (\s1 s0n l -> first (,s1) s0n : l) []

    uc = uncurry
    err' = err . ("enumMutations: " ++)
    assertEq n n'
      | n /= n' = err' $ "should be eq: " ++ show (n,n')
      | otherwise = n

-- WHERE --
byFstToMap :: IntMap (IntMap Int) -> Map (Sym,Sym) Int
byFstToMap = M.fromDistinctAscList . byFstToAscList

byFstToAscList :: IntMap (IntMap Int) -> [((Sym,Sym),Int)]
byFstToAscList = concatMap (\(s0,im) -> first (s0,)
                                        <$> IM.toAscList im)
                 . IM.toAscList

bySndToMap :: IntMap (IntMap Int) -> Map (Sym,Sym) Int
bySndToMap = M.fromDistinctAscList . bySndToAscList

bySndToAscList :: IntMap (IntMap Int) -> [((Sym,Sym),Int)]
bySndToAscList = fmap (first swap) . byFstToAscList
--

-------------------
-- EVAL MUTATION --
-------------------

-- | Compute the loss incurred by the application of a mutation
evalMutation :: Int -> Int -> U.Vector Int -> IntMap Int -> Int -> Int ->
                MutationJoints -> Double
evalMutation m bigN ns ns' nm vm = go
  where
    deltaDelta_ :: Int -> [(Int,Int)] -> Int -> Double
    deltaDelta_ nm' dns vm' = deltaDelta m bigN (nm,nm') dns (vm,vm')

    go :: MutationJoints -> Double
    go (MutD (AddLeft s0) jtns) = goAdd1 s0 jtns
    go (MutD (AddRight s1) jtns) = goAdd1 s1 jtns
    go (MutD (Add2 s0 s1) n01) = deltaDelta_ nm' dns (vm+2)
      where nm' = nm + n01
            n0' = IM.findWithDefault (ns U.! s0) s0 ns'
            n1' = IM.findWithDefault (ns U.! s1) s1 ns'
            dns | s0 == s1  = [(n0', n0'-2*n01)]
                | otherwise = [(n0', n0'-n01)
                              ,(n1', n1'-n01)]

    go (MutD (DelLeft s0) jtns) = goDel1 s0 jtns
    go (MutD (DelRight s1) jtns) = goDel1 s1 jtns
    go (MutD (Del2 s0 s1) n01) = deltaDelta_ nm' dns (vm-2)
      where
        nm' = nm - n01
        n0 = ns' IM.! s0
        n1 = ns' IM.! s1
        dns | s0 == s1  = [(n0, n0+2*n01)]
            | otherwise = [(n0, n0+n01)
                          ,(n1, n1+n01)]

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goAdd1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm + dnm
        ns'' = IM.insertWith (\_ n0' -> n0') s0 (ns U.! s0) ns'
               -- ^ ensures s0 is in ns' before intersection operation
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'-adn)) ns'' adns
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns' because
              -- each key of jtns is a staged symbol

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goDel1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm - dnm
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'+adn)) ns' adns
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'

-- | Computes the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDelta :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> Double
deltaDelta m bigN (nm,nm') dns (vm,vm') =
  nDeltaDelta + sDeltaDelta + rDeltaDelta
  where
    nDeltaDelta = logFactNpmmnm' - logFactNpmmnm
    logFactNpmmnm = logFact $ mpN - nm
    logFactNpmmnm' = logFact $ mpN - nm'
    mpN = m + bigN -- m + N

    sDeltaDelta = sDDltm + logFact nm - logFact nm'
    sDDltm = sum $ (<$> dns) $ \(ni',ni'') ->
      logFact ni' - logFact ni''
      -- (`logFact ni` (old symbol count) cancel out)

    rDeltaDelta = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral

--------------------
-- APPLY MUTATION --
--------------------

appMutation :: Monad m => Mutation k -> RefinementT m ()
appMutation mutation = case mutation of
  AddLeft s0 -> do
    -- (Out,In): remove (Out[s0] <--> In[ins])
    ins <- membership.byFstOutIn %%= deleteFind s0
    zoom membership $ do
      bySndInOut %= const (IM.delete s0) `atEvery` ins

      -- (Out,Out): remove (Out[s0] <--> Out[outs])
      outs <- byFstOutOut %%= deleteFind s0
      bySndOutOut %= const (IM.delete s0) `atEvery` outs

      -- (In,In): insert (In[s0] <--> In[ins])
      byFstInIn %= IM.insertWithKey (col "byFstInIn") s0 ins
      bySndInIn %= IM.insertWithKey (col "bySndInIn") s0 `atEvery` ins

      -- (In,Out): insert (In[s0] <--> Out[outs])
      byFstInOut %= IM.insertWithKey (col "byFstInOut") s0 outs
      bySndOutIn %= IM.insertWithKey (col "bySndOutIn") s0 `atEvery` outs

    let dnm = sum ins
    jointCount += dnm
    refinement.left %= UT.insertMissing s0

    -- (insert s0 if not already in ns')
    ns <- use $ modelParams.symbolCounts
    newSymbolCounts %= IM.insertWith (\_ n0' -> n0') s0 (ns U.! s0)
    let adns = IM.insertWith (+) s0 dnm ins -- absolute differences
    newSymbolCounts %= (\_ adn n -> Just (n-adn)) `modifyEvery` adns
      where
        col str k a a' = error $ "AddLeft (" ++ str ++ "):\n"
                         ++ "  collision: " ++ show (k,(a,a'))

  AddRight s1 -> do
    -- (In,Out): remove (In[ins] <--> Out[s1])
    ins <- membership.bySndOutIn %%= deleteFind s1
    zoom membership $ do
      byFstInOut %= const (IM.delete s1) `atEvery` ins

      -- (Out,Out): remove (Out[outs] <--> Out[s1])
      outs <- bySndOutOut %%= deleteFind s1
      byFstOutOut %= const (IM.delete s1) `atEvery` outs

      -- (In,In): insert (In[ins] <--> In[s1])
      bySndInIn %= IM.insertWithKey (col "bySndInIn") s1 ins
      byFstInIn %= IM.insertWithKey (col "byFstInIn") s1 `atEvery` ins

      -- (Out,In): insert (Out[outs] <--> In[s1])
      bySndInOut %= IM.insertWithKey (col "bySndInOut") s1 outs
      byFstOutIn %= IM.insertWithKey (col "byFstOutIn") s1 `atEvery` outs

    let dnm = sum ins
    jointCount += dnm
    refinement.right %= UT.insertMissing s1

    -- (insert s1 if not already in ns')
    ns <- use $ modelParams.symbolCounts
    newSymbolCounts %= IM.insertWith (\_ n1' -> n1') s1 (ns U.! s1)
    let adns = IM.insertWith (+) s1 dnm ins -- absolute differences
    newSymbolCounts %= (\_ adn n -> Just (n-adn)) `modifyEvery` adns
      where
        col str k a a' = error $ "AddRight (" ++ str ++ "):\n"
                         ++ "  collision: " ++ show (k,(a,a'))

  Add2 s0 s1 -> do
    -- (Out,Out): remove (Out[s0] <--> Out[out0s])
    out0s <- membership.byFstOutOut %%= deleteFind s0
    let n01 = out0s IM.! s1

    zoom membership $ do
      bySndOutOut %= const (IM.delete s0) `atEvery` out0s
      -- (Out,Out): remove (Out[out1s] <--> Out[s1])
      out1s <- bySndOutOut %%= deleteFind s1
      byFstOutOut %= const (IM.delete s1) `atEvery` out1s

      -- [no ins prior to Add2 by definition]
      let in0s = IM.singleton s1 n01
          in1s = IM.singleton s0 n01

      -- (In,In): insert (In[s0] <--> In[s1])
      byFstInIn %= IM.insertWithKey (col "byFstInIn") s0 in0s
      bySndInIn %= IM.insertWithKey (col "bySndInIn") s1 in1s

      -- (In,Out): insert (In[s0] <--> Out[out0s])
      byFstInOut %= IM.insertWithKey (col "byFstInOut") s0 out0s
      bySndOutIn %= IM.insertWithKey (col "bySndOutIn") s0 `atEvery` out0s

      -- (Out,In): insert (Out[out1s] <--> In[s1])
      bySndInOut %= IM.insertWithKey (col "bySndInOut") s1 out1s
      byFstOutIn %= IM.insertWithKey (col "byFstOutIn") s1 `atEvery` out1s

    jointCount += n01
    refinement.left %= UT.insertMissing s0
    refinement.right %= UT.insertMissing s1
    ns <- use $ modelParams.symbolCounts
    n0' <- newSymbolCounts `uses` IM.findWithDefault (ns U.! s0) s0
    n1' <- newSymbolCounts `uses` IM.findWithDefault (ns U.! s1) s1
    newSymbolCounts %= if s0 == s1 then IM.insert s0 (n0'-2*n01)
                       else IM.insert s0 (n0'-n01) . IM.insert s1 (n1'-n01)
      where
        col str k a a' = error $ "Add2 (" ++ str ++ "):\n"
                         ++ "  collision: " ++ show (k,(a,a'))

  DelLeft s0 -> do
    -- (In,In): remove (In[s0] <--> In[ins])
    ins <- membership.byFstInIn %%= deleteFind s0
    zoom membership $ do
      bySndInIn %= const (IM.delete s0) `atEvery` ins

      -- (In,Out): remove (In[s0] <--> Out[outs])
      outs <- byFstInOut %%= deleteFind s0
      bySndOutIn %= const (IM.delete s0) `atEvery` outs

      -- (Out,Out): insert (Out[s0] <--> Out[outs])
      byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s0 outs
      bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s0 `atEvery` outs

      -- (Out,In): insert (Out[s0] <--> In[ins])
      byFstOutIn %= IM.insertWithKey (col "byFstOutIn") s0 ins
      bySndInOut %= IM.insertWithKey (col "bySndInOut") s0 `atEvery` ins

    let dnm = sum ins
    jointCount -= dnm
    refinement.left %= UT.delete s0

    ns <- use $ modelParams.symbolCounts
    let adns = IM.insertWith (+) s0 dnm ins -- absolute differences
    newSymbolCounts %= (\s adn n -> nothingIf (ns U.! s ==) (n+adn))
                       `modifyEvery` adns
      where
        col str k a a' = error $ "DelLeft (" ++ str ++ "):\n"
                         ++ "  collision: " ++ show (k,(a,a'))

  DelRight s1 -> do
    -- (In,In): remove (In[ins] <--> In[s1])
    ins <- membership.bySndInIn %%= deleteFind s1
    zoom membership $ do
      byFstInIn %= const (IM.delete s1) `atEvery` ins

      -- (Out,In): remove (Out[outs] <--> In[s1])
      outs <- bySndInOut %%= deleteFind s1
      byFstOutIn %= const (IM.delete s1) `atEvery` outs

      -- (Out,Out): insert (Out[outs] <--> Out[s1])
      bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s1 outs
      byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s1 `atEvery` outs

      -- (In,Out): insert (In[ins] <--> Out[s1])
      bySndOutIn %= IM.insertWithKey (col "bySndOutIn") s1 ins
      byFstInOut %= IM.insertWithKey (col "byFstInOut") s1 `atEvery` ins

    let dnm = sum ins
    jointCount -= dnm
    refinement.right %= UT.delete s1

    ns <- use $ modelParams.symbolCounts
    let adns = IM.insertWith (+) s1 dnm ins -- absolute differences
    newSymbolCounts %= (\s adn n -> nothingIf (ns U.! s ==) (n+adn))
                       `modifyEvery` adns
        where
          col str k a a' = error $ "DelRight (" ++ str ++ "):\n"
                           ++ "  collision: " ++ show (k,(a,a'))

  Del2 s0 s1 -> do
    -- (In,In): remove (In[s0] --> In[s1])
    n01 <- membership.byFstInIn %%= first (head . IM.elems)
                                . deleteFind s0 -- singleton by definition
    zoom membership $ do
      -- (In,Out): remove (In[s0] <--> Out[out0s])
      out0s <- byFstInOut %%= deleteFind s0
      bySndOutIn %= const (IM.delete s0) `atEvery` out0s

      -- (Out,In): remove (Out[out1s] <--> In[s1])
      out1s <- bySndInOut %%= deleteFind s1
      byFstOutIn %= const (IM.delete s1) `atEvery` out1s

      -- (In,In): remove (In[s0] <-- In[s1])
      bySndInIn %= IM.delete s1 -- singleton by definition

      let out0s' = IM.insertWithKey (col "s0-->s1") s1 n01 out0s
          out1s' = IM.insertWithKey (col "s0<--s1") s0 n01 out1s

      -- (Out,Out): insert (Out[s0] <--> Out[out0s])
      byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s0 out0s'
      bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s0 `atEvery` out0s
      -- (Out,Out): insert (Out[out1s] <--> Out[s1])
      bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s1 out1s'
      byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s1 `atEvery` out1s

    jointCount -= n01
    refinement.left %= UT.delete s0
    refinement.right %= UT.delete s1

    ns <- use $ modelParams.symbolCounts
    n0' <- newSymbolCounts `uses` (IM.! s0)
    n1' <- newSymbolCounts `uses` (IM.! s1)
    let n0'' = n0' + n01
        n1'' = n1' + n01
        op0 | n0'' == (ns U.! s0) = IM.delete s0
            | otherwise = IM.insert s0 n0''
        op1 | n1'' == (ns U.! s1) = IM.delete s1
            | otherwise = IM.insert s1 n1''
    newSymbolCounts %= if s0 == s1 then IM.delete s0 else op0 . op1
      where
        col str k a a' = error $ "Del2 (" ++ str ++ "):\n"
                         ++ "  collision: " ++ show (k,(a,a'))

-- WHERE --
deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)

deleteFind :: Sym -> IntMap a -> (a, IntMap a)
deleteFind = first fromJust .: IM.updateLookupWithKey (\_ _ -> Nothing)

-- | Lift a function to inner-maps located at a set of indexes
atEvery :: forall a b. (b -> IntMap a -> IntMap a) -> IntMap b ->
           IntMap (IntMap a) -> IntMap (IntMap a)
atEvery f = IM.mergeWithKey g h id
  where
    -- apply f to map (ignore index), Nothing if result is null
    g :: Int -> b -> IntMap a -> Maybe (IntMap a)
    g _ = nothingIf IM.null .: f
    -- apply f to every value in the key map with an empty map
    h :: IntMap b -> IntMap (IntMap a)
    h = fmap (`f` IM.empty)

-- | @(f `modifyEvery` bs) as@ applies @f b a@ to every element in @bs@
-- inside the map @as@ (deletes if Nothing) and raises an error if a key
-- of @bs@ is missing in @as@
modifyEvery :: Show b => (Sym -> b -> a -> Maybe a) -> IntMap b -> IntMap a -> IntMap a
modifyEvery f = IM.mergeWithKey f
                (error . ("modifyEvery: missing keys: " ++) . show) id
--

---------------
-- EXECUTION --
---------------

stepHillClimb :: PrimMonad m => RefinementT m (Maybe JointType)
stepHillClimb = do
  (minLoss, MutD mut _) <- minMutation
  if minLoss >= 0 then refinement `uses` Just -- end
    else appMutation mut -- step
         >> return Nothing

hillClimb :: PrimMonad m => RefinementT m JointType
hillClimb = (stepHillClimb >>=) $ \case
  Nothing -> hillClimb
  Just jt -> return jt

minMutation :: PrimMonad m => RefinementT m ( Double
                                            , MutationJoints )
minMutation = do
  muts <- membership `uses` enumMutations
  (RefinementState (Params m bigN ns) _ nm rjt ns' _) <- get
  let vm = JT.variety rjt
      emuts = toFst (evalMutation m bigN ns ns' nm vm) <$> muts
  return $ L.minimumOn fst emuts

negMutations :: PrimMonad m => RefinementT m [( Double
                                              , MutationJoints)]
negMutations = do
  muts <- membership `uses` enumMutations
  (RefinementState (Params m bigN ns) _ nm rjt ns' _) <- get
  let vm = JT.variety rjt
      emuts = toFst (evalMutation m bigN ns ns' nm vm) <$> muts
  return $ L.filter ((<0) . fst) emuts

-- stepEM :: Monad m => RefinementT m (Maybe JointType)
-- stepEM = do
--   muts <- negMutations
--   if null muts then refinement `uses` Just -- end
--     else mapM_ (appMutation . snd) muts -- step
--          >> return Nothing

-- optimize_ :: Int -> Int -> U.Vector Int -> IntMap (IntMap Int) ->
--              IntMap (IntMap Int) -> JointType -> JointType
-- optimize_ m bigN ns byFst_0 bySnd_0 (JT ru0_0 ru1_0) = undefined
--   where
--     mpN = m + bigN

--     -- from Diagram.Model ------------------------------------
--     mLen_ = eliasCodeLen . (+(-256))
--     mDelta_ = mLen_ (m+1) - mLen_ m
--     tsLen_ = m*m - m - 65280
--     tsLen_' = (m+1)*(m+1) - (m+1) - 65280
--     tsDelta_ = tsLen_' - tsLen_
--     bigNDelta_ k = eliasCodeLen (bigN - k) - eliasCodeLen bigN
--     mtnDelta k = mDelta_ + tsDelta_ + bigNDelta_ k
--     ----------------------------------------------------------

--     logm = ilog m -- log(m)
--     logFactNpmm1 = logFact $ mpN - 1 -- log((N + m - 1)!)
--     logFactNpmmnm_0 = logFact $ mpN - nm_0 -- log((N + m - nm)!)
--     nm_0 = undefined -- sum $ sum <$> byFstInIn_0

--     logFactnm_0 = logFact nm_0

--     vm_0 = UT.length ru0_0 + UT.length ru1_0
--     rInfo_0 = fromIntegral nm_0 * ilog vm_0

--------------
-- IO STATS --
--------------

printInfo :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
             (JointType, Map (Sym,Sym) b) -> m ()
printInfo (jt,jts) (rjt,rjts) = liftIO $ putStrLn $
  "generated refinement type with size "
  ++ show (JT.size rjt)
  ++ " from "  ++ show (JT.size jt)
  ++ " covering " ++ show (Jts.size rjts)
  ++ " joints out of " ++ show (Jts.size jts)
  ++ " ("  ++ show
  (round @_ @Int $ 100.0 * fromIntegral (Jts.size rjts)
    / fromIntegral @_ @Double (Jts.size jts))
  ++ "%)"

printLUB :: MonadIO m => JointType -> Map (Sym,Sym) a -> m ()
printLUB jt jts = liftIO $ do
  putStr "refinement is "
  if jt == JT.fromJoints jts
    then putStrLn $ inGreen "LUB" ++ " of its joints"
    else do putStrLn $ inRed "not LUB" ++ " of its joints"
            putStrLn $ "rtjt: " ++ show (jt, void jts)
            error "LUB error"

printSubtyping :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                  (JointType, Map (Sym,Sym) b) -> m ()
printSubtyping (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "refinement is "
  if rjt `JT.leq` jt
    then putStrLn $ inGreen "subtype" ++ " of its parent"
    else do putStrLn $ inRed "not subtype" ++ " of its parent"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "subtype error"

printConservation :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                     (JointType, Map (Sym,Sym) a) -> m ()
printConservation (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "split " -- TODO: check disjointness too?
  if void jts == (void rjts `M.union` void jts')
    then putStrLn $ inGreen "preserves" ++ " all joints"
    else do putStrLn $ inRed "does not preserve" ++ " all joints"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "joints split error"

printMembership :: MonadIO m => Map (Sym,Sym) a -> (JointType, Map (Sym,Sym) a) -> m ()
printMembership jts (rjt,rjts) = liftIO $ do
  let rjtsVerif = M.filterWithKey (\k _ -> k `JT.member` rjt) jts
  putStr "returned joints "
  if M.keys rjts == M.keys rjtsVerif
    then putStrLn $ inGreen "match" ++ " joints covered by the refinement"
    else do putStrLn $ inRed "don't match" ++ " joints covered by the refinement"
            putStrLn $ "rtjt: " ++ show (M.keys rjts)
              ++ "\nrjts: " ++ show (M.keys rjts)
              ++ "\nrjtsVerif: " ++ show (M.keys rjtsVerif)
            error "joints coverage error"

inRed :: String -> String
inRed s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

inGreen :: String -> String
inGreen s = "\ESC[32m" ++ s ++ "\ESC[0m"
