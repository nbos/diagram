{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TupleSections, LambdaCase, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.Refinement (module Diagram.Refinement) where

import Control.Monad as Monad
import Control.Lens hiding (both,last1)
import Control.Monad.State.Strict
import Control.Monad.Random

import Data.Maybe
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as U

import Diagram.Primitive
import Diagram.Information

import Diagram.UnionType (Sym)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(..))

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
-- >>> uncurry enumRefinements $
--     Jts.byFstSized &&& Jts.bySndSized 256 $ Jts.fromList [1,2,3,2]
-- [ fromLists ([],[])
-- , fromLists ([3],[2])
-- , fromLists ([2],[3])
-- , fromLists ([2,3],[2,3])
-- , fromLists ([1],[2])
-- , fromLists ([1,3],[2])
-- , fromLists ([1,2],[2,3])
-- , fromLists ([1,2,3],[2,3]) ]
enumRefinements :: forall a. Map Sym (Map Sym a) ->
  Map Sym (Map Sym a) -> [JointType]
enumRefinements byFst0 bySnd0 = concatMap givenU0 u0s
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
  _jtsByFst :: !(Map Sym ([Sym], Map Sym a)),
  _jtsBySnd :: !(Map Sym ([Sym], Map Sym a)),
  _fstUnion :: !IntSet,
  _sndUnion :: !IntSet,
  _refJoints :: !(Map (Sym,Sym) ())
}
makeLenses ''GenerationState

-- | Generate a random refinement, given a set of joints indexed both
-- ways
genRefinement :: (MonadRandom m, PrimMonad m) => Map Sym (Map Sym a) ->
                 Map Sym (Map Sym a) -> m (JointType, Map (Sym,Sym) ())
genRefinement = genRefinementWith 0.5

-- | Generate a random refinement, given a sampling probability
genRefinementWith :: forall m a. (MonadRandom m, PrimMonad m) =>
  Double -> Map Sym (Map Sym a) -> Map Sym (Map Sym a) -> m ( JointType
                                                            , Map (Sym,Sym) () )
genRefinementWith r byFst0 bySnd0 =
  evalStateT go $ GenerationState
  (([],) <$> byFst0)
  (([],) <$> bySnd0) IS.empty IS.empty M.empty
  where
    go :: StateT (GenerationState a) m (JointType, Map (Sym,Sym) ())
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
        forM_ staged0 $ \s1 -> -- add staged Joints
          refJoints %= M.insert (s0,s1) ()

      -- unlink from neighbors
      deleted1 <- forM (M.keys jt0s) $ \s1 -> do
        jtsBySnd . at s1 %%= \case
          Nothing -> error "impossible"
          Just (staged1, jt1s) -- -> (deleted, inserted)
            | null staged1' && M.null jt1s' -> (Just s1, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged1', jt1s')) -- update
            where
              staged1' | sel0 = s0:staged1 -- stage s0 on s1 if selected
                       | otherwise = staged1
              jt1s' = M.delete s0 jt1s

      -- enforce invariant if necessary
      when (sel0 && null staged0) $ do
        i <- getRandomR (0, M.size jt0s - 1) -- select a symbol
        let (s1, _) = M.elemAt i jt0s
        if Just s1 `notElem` deleted1 then goElimSnd True s1 -- rec
          else do sndUnion %= IS.insert s1
                  refJoints %= M.insert (s0,s1) () -- null staged1

    -- | Symmetric with above, could probably be factored into one, but
    -- ehhh
    goElimSnd :: Bool -> Int -> StateT (GenerationState a) m ()
    goElimSnd sel1 s1 = do
      (staged1, jt1s) <- jtsBySnd %%= deleteFind s1 -- remove from avail.
      when sel1 $ do
        sndUnion %= IS.insert s1 -- add to JointType
        forM_ staged1 $ \s0 -> -- add staged Joints
          refJoints %= M.insert (s0,s1) ()

      -- unlink from neighbors
      deleted0 <- forM (M.keys jt1s) $ \s0 -> do
        jtsByFst . at s0 %%= \case
          Nothing -> error "impossible"
          Just (staged0, jt0s) -- -> (deleted, inserted)
            | null staged0' && M.null jt0s' -> (Just s0, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged0', jt0s')) -- update
            where
              staged0' | sel1 = s1:staged0 -- stage s1 on s0 if selected
                       | otherwise = staged0
              jt0s' = M.delete s1 jt0s

      -- enforce invariant if necessary
      when (sel1 && null staged1) $ do
        i <- getRandomR (0, M.size jt1s - 1) -- select a symbol
        let (s0, _) = M.elemAt i jt1s
        if Just s0 `notElem` deleted0 then goElimFst True s0 -- rec
          else do fstUnion %= IS.insert s0
                  refJoints %= M.insert (s0,s1) () -- null staged0

-- -- | (DO NOT USE (NAIVE)) Generate a random refinement of a type. The
-- -- produced type is not necessarily a valid refinement (least upper
-- -- bound) of the set of joints it covers.
-- genRefinement_ :: MonadRandom m => JointType -> m JointType
-- genRefinement_ (JT u0 u1) = do
--   (_,ss0) <- R.split (UT.toAscList u0)
--   (_,ss1) <- R.split (UT.toAscList u1)
--   return $ JT (UT.fromDistinctAscList ss0) (UT.fromDistinctAscList ss1)

-- --------------- --
-- -- MUTATIONS -- --
-- --------------- --

data Mutation
  = AddLeft  !Sym !(IntMap Int)
  | AddRight !Sym !(IntMap Int)
  | Add2     !Sym !Sym !Int
  | DelLeft  !Sym !(IntMap Int)
  | DelRight !Sym !(IntMap Int)
  | Del2     !Sym !Sym !Int

data ModelParams = Params
  { _numSymbols :: !Int
  , _stringLength :: !Int
  , _symbolCounts :: !(U.Vector Int) }

type RefinementT = StateT RefinementState
data RefinementState = RefinementState
  { _modelParams :: !ModelParams -- for params: m, N, ns
  -- , _parent :: !JointType
  , _jointCount :: !Int -- :: nm
  , _refinement :: !JointType -- :: rjt
  , _newSymbolCounts :: !(IntMap Int) -- :: ns'
  , _joints :: !CoverageState }

data CoverageState = CoverageState
  { _byFstInIn   :: !(IntMap (IntMap Int))
  , _byFstInOut  :: !(IntMap (IntMap Int))
  , _byFstOutIn  :: !(IntMap (IntMap Int))
  , _byFstOutOut :: !(IntMap (IntMap Int))
  , _bySndInIn   :: !(IntMap (IntMap Int))
  , _bySndInOut  :: !(IntMap (IntMap Int))
  , _bySndOutIn  :: !(IntMap (IntMap Int))
  , _bySndOutOut :: !(IntMap (IntMap Int)) }

makeLenses ''ModelParams
makeLenses ''RefinementState
makeLenses ''CoverageState

-- | Enumerate and evaluate all possible mutations. Apply the one with
-- the lowest loss if negative, returning Nothing. Returns Just if the
-- JointType cannot be improved further (all mutations have positive
-- loss).
stepHillClimb :: RefinementT m (Maybe JointType)
stepHillClimb = undefined

initRefinementState :: ModelParams -> IntMap (IntMap Int) ->
                       IntMap (IntMap Int) -> JointType -> RefinementState
initRefinementState mdl@(Params _ _ ns) byFst bySnd rjt =
  RefinementState { _modelParams = mdl
                  , _jointCount = nm
                  , _refinement = rjt
                  , _newSymbolCounts = ns'
                  , _joints = coverage }
  where
    coverage = initCoverageState byFst bySnd rjt
    nm = sum $ snd <$> byFstInInL
    byFstInInL = byFstToAscList $ coverage ^. byFstInIn
    ns' = IM.mapWithKey (\s dn -> (ns U.! s) - dn ) $
            IM.fromListWith (+) $
            foldr (\((s0,s1),n01) l -> (s0,n01):(s1,n01):l)
            [] byFstInInL

initCoverageState :: IntMap (IntMap Int) -> IntMap (IntMap Int) ->
                     JointType -> CoverageState
initCoverageState byFst bySnd (JT u0 u1) =
  CoverageState { _byFstInIn   = byFstInIn_
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

--------------------------
-- MUTATION ENUMERATION --
--------------------------

enumMutations :: Monad m => RefinementT m [Mutation]
enumMutations = do
  (CoverageState byFstInIn_ _ byFstOutIn_ byFstOutOut_
   bySndInIn_ _ bySndOutIn_ _) <- use joints

  let als = uc AddLeft  <$> IM.toList byFstOutIn_ -- Out --> In
      ars = uc AddRight <$> IM.toList bySndOutIn_ -- In <-- Out

      -- (Out,Out) that can't be added one-by-one
      a2s = uc (uc Add2) <$> M.toList outSubgraph
      outSubgraph = byFstToMap $
                    (IM.\\ bySndOutIn_)
                    <$> (byFstOutOut_ IM.\\ byFstOutIn_)

      leftDependents = IM.mapMaybe fromSingleton byFstInIn_
      rightDependents = IM.mapMaybe fromSingleton bySndInIn_
      fromSingleton im | [sn] <- IM.toList im = Just sn
                       | otherwise = Nothing

      -- can be deleted yet: In (w/o dependents) --> In
      dls = uc DelLeft <$> byFstInInWithoutDeps
      byFstInInWithoutDeps = filter (IM.disjoint rightDependents . snd) $
                             IM.toList byFstInIn_ -- In --> In

      -- can be deleted yet: In <-- In (w/o dependents)
      drs = uc DelRight <$> bySndInInWithoutDeps
      bySndInInWithoutDeps = filter (IM.disjoint leftDependents . snd) $
                             IM.toList bySndInIn_ -- In <-- In

      -- (In,In) that can't be deleted one-by-one
      d2s = uc (uc Del2) <$> M.toList coDependents
      coDependents = M.intersectionWith assertEq
                     leftPendantJts rightPendantJts

      leftPendantJts = M.fromDistinctAscList $ toList leftDependents
        where toList :: IntMap (s1, a) -> [((Sym, s1), a)]
              toList = IM.foldrWithKey (\s0 s1n l -> first (s0,) s1n : l) []

      rightPendantJts = M.fromDistinctAscList $ toList rightDependents
        where toList :: IntMap (s0, a) -> [((s0, Sym), a)]
              toList = IM.foldrWithKey (\s1 s0n l -> first (,s1) s0n : l) []

  return $ als ++ ars ++ a2s ++ dls ++ drs ++ d2s
    where
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

-------------------------
-- MUTATION EVALUATION --
-------------------------

-- | Evaluate the loss incurred by the application of a mutation
evalMutation :: Int -> Int -> U.Vector Int -> IntMap Int -> Int -> Int ->
                Mutation -> Double
evalMutation m bigN ns ns' nm vm = go
  where
    deltaDelta_ :: Int -> [(Int,Int)] -> Int -> Double
    deltaDelta_ nm' dns vm' = deltaDelta m bigN (nm,nm') dns (vm,vm')

    go :: Mutation -> Double
    go (AddLeft s0 jtns) = goAdd1 s0 jtns
    go (AddRight s1 jtns) = goAdd1 s1 jtns
    go (Add2 s0 s1 n01) = deltaDelta_ nm' dns (vm+2)
      where nm' = nm + n01
            n0 = ns U.! s0 -- not in ns' by Add2 definition
            n1 = ns U.! s1 -- not in ns' by Add2 definition
            dns | s0 == s1  = [(n0,n0-2*n01)]
                | otherwise = [(n0,n0-n01),(n1,n1-n01)]

    go (DelLeft s0 jtns) = goDel1 s0 jtns
    go (DelRight s1 jtns) = goDel1 s1 jtns
    go (Del2 s0 s1 n01) = deltaDelta_ (nm-n01) dns (vm-2)
      where
        dns | s0 == s1  = [(2*n01,0)]
            | otherwise = [(n01,0),(n01,0)]
            -- n0'',n1'' = 0 by Del2 definition

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goAdd1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm + dnm
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\adn n' -> (n', n'-adn)) adns $
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'
              -- (because each key of jtns is a staged symbol)
              IM.insertWith (\_ n0' -> n0') s0 (ns U.! s0) ns'
              -- updates on s0 handle case where s0 also appears
              -- (or doesn't) in jtns (i.e. on the other side)

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goDel1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm - dnm
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\adn n' -> (n', n'+adn)) adns $
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'
              IM.insertWith (\_ n0' -> n0') s0 (ns U.! s0) ns'
              -- updates on s0 handle case where s0 also appears
              -- (or doesn't) in jtns (i.e. on the other side)

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
      -- (logFact ni (old symbol count) cancel out)

    rDeltaDelta = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral

--------------------------
-- MUTATION APPLICATION --
--------------------------

appMutation :: Monad m => Mutation -> RefinementT m ()
appMutation mutation = zoom joints $ case mutation of
  (AddLeft s0 _) -> do
    -- (Out,Out): remove (Out[s0] <--> Out[outs])
    outs <- byFstOutOut %%= deleteFind s0
    bySndOutOut %= const (IM.delete s0) `atEvery` outs

    -- (Out,In): remove (Out[s0] <--> In[ins])
    ins <- byFstOutIn %%= deleteFind s0
    bySndInOut %= const (IM.delete s0) `atEvery` ins

    -- (In,In): insert (In[s0] <--> In[ins])
    byFstInIn %= IM.insertWithKey (col "byFstInIn") s0 ins
    bySndInIn %= IM.insertWithKey (col "bySndInIn") s0 `atEvery` ins

    -- (In,Out): insert (In[s0] <--> Out[outs])
    byFstInOut %= IM.insertWithKey (col "byFstInOut") s0 outs
    bySndOutIn %= IM.insertWithKey (col "bySndOutIn") s0 `atEvery` outs
    where
      col str k a a' = error $ "AddLeft (" ++ str ++ "):\n"
                       ++ "  collision: " ++ show (k,(a,a'))

  (AddRight s1 _) -> do
    -- (Out,Out): remove (Out[outs] <--> Out[s1])
    outs <- bySndOutOut %%= deleteFind s1
    byFstOutOut %= const (IM.delete s1) `atEvery` outs

    -- (In,Out): remove (In[ins] <--> Out[s1])
    ins <- bySndOutIn %%= deleteFind s1
    byFstInOut %= const (IM.delete s1) `atEvery` ins

    -- (In,In): insert (In[ins] <--> In[s1])
    bySndInIn %= IM.insertWithKey (col "bySndInIn") s1 ins
    byFstInIn %= IM.insertWithKey (col "byFstInIn") s1 `atEvery` ins

    -- (Out,In): insert (Out[outs] <--> In[s1])
    bySndInOut %= IM.insertWithKey (col "bySndInOut") s1 outs
    byFstOutIn %= IM.insertWithKey (col "byFstOutIn") s1 `atEvery` outs
    where
      col str k a a' = error $ "AddRight (" ++ str ++ "):\n"
                       ++ "  collision: " ++ show (k,(a,a'))

  (Add2 s0 s1 n01) -> do
    -- (Out,Out): remove (Out[s0] <--> Out[out0s])
    out0s <- byFstOutOut %%= deleteFind s0
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
    where
      col str k a a' = error $ "Add2 (" ++ str ++ "):\n"
                       ++ "  collision: " ++ show (k,(a,a'))

  (DelLeft s0 _) -> do
    -- (In,Out): remove (In[s0] <--> Out[outs])
    outs <- byFstInOut %%= deleteFind s0
    bySndOutIn %= const (IM.delete s0) `atEvery` outs

    -- (In,In): remove (In[s0] <--> In[ins])
    ins <- byFstInIn %%= deleteFind s0
    bySndInIn %= const (IM.delete s0) `atEvery` ins

    -- (Out,Out): insert (Out[s0] <--> Out[outs])
    byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s0 outs
    bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s0 `atEvery` outs

    -- (Out,In): insert (Out[s0] <--> In[ins])
    byFstOutIn %= IM.insertWithKey (col "byFstOutIn") s0 ins
    bySndInOut %= IM.insertWithKey (col "bySndInOut") s0 `atEvery` ins
    where
      col str k a a' = error $ "DelLeft (" ++ str ++ "):\n"
                       ++ "  collision: " ++ show (k,(a,a'))

  (DelRight s1 _) -> do
    -- (Out,In): remove (Out[outs] <--> In[s1])
    outs <- bySndInOut %%= deleteFind s1
    byFstOutIn %= const (IM.delete s1) `atEvery` outs

    -- (In,In): remove (In[ins] <--> In[s1])
    ins <- bySndInIn %%= deleteFind s1
    byFstInIn %= const (IM.delete s1) `atEvery` ins

    -- (Out,Out): insert (Out[outs] <--> Out[s1])
    bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s1 outs
    byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s1 `atEvery` outs

    -- (In,Out): insert (In[ins] <--> Out[s1])
    bySndOutIn %= IM.insertWithKey (col "bySndOutIn") s1 ins
    byFstInOut %= IM.insertWithKey (col "byFstInOut") s1 `atEvery` ins
    where
      col str k a a' = error $ "DelRight (" ++ str ++ "):\n"
                       ++ "  collision: " ++ show (k,(a,a'))

  (Del2 s0 s1 n01) -> do
    -- (In,Out): remove (In[s0] <--> Out[out0s])
    out0s <- byFstInOut %%= deleteFind s0
    bySndOutIn %= const (IM.delete s0) `atEvery` out0s

    -- (Out,In): remove (Out[out1s] <--> In[s1])
    out1s <- bySndInOut %%= deleteFind s1
    byFstOutIn %= const (IM.delete s1) `atEvery` out1s

    -- (In,In): remove (In[s0] <--> In[s1])
    byFstInIn %= IM.delete s0 -- singleton by definition
    bySndInIn %= IM.delete s1 -- singleton by definition

    let out0s' = IM.insertWithKey (col "s0-->s1") s1 n01 out0s
        out1s' = IM.insertWithKey (col "s0<--s1") s0 n01 out1s

    -- (Out,Out): insert (Out[s0] <--> Out[out0s])
    byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s0 out0s'
    bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s0 `atEvery` out0s
    -- (Out,Out): insert (Out[out1s] <--> Out[s1])
    bySndOutOut %= IM.insertWithKey (col "bySndOutOut") s1 out1s'
    byFstOutOut %= IM.insertWithKey (col "byFstOutOut") s1 `atEvery` out1s
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
--

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
