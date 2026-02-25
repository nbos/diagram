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
import qualified Data.List.Extra as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as U

import Diagram.Primitive
import Diagram.Information
import qualified Diagram.Random as R
import qualified Diagram.Dynamic as Dyn

import Diagram.UnionType (Sym)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import Diagram.Model (Model(..))
import qualified Diagram.Model as Mdl

import Diagram.Util

err :: String -> a
err = error . ("Refinement." ++)

-----------------------
-- RANDOM GENERATION --
-----------------------

-- | O(n^2) Generate all combinations
combs :: [a] -> [[a]]
combs [] = [[]]
combs (a:as) = ass ++ fmap (a:) ass
  where ass = combs as

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
        | total == 0 -> return ( JT (UT.fromSet u0) (UT.fromSet u1)
                               , ref ) -- end
        | otherwise -> do
            i <- getRandomR (0, total-1) -- select a symbol
            f <- getRandom @_ @Double -- include/exclude it in the ref
            let b = f <= r
            if i < len0 then goElimFst b (fst $ M.elemAt i byFst)
              else goElimSnd b (fst $ M.elemAt (i - len0) bySnd)
            go -- rec
        where
          len0 = M.size byFst -- O(1)
          len1 = M.size bySnd -- O(1)
          total = len0 + len1

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

-- | (DO NOT USE (NAIVE)) Generate a random refinement of a type. The
-- produced type is not necessarily a valid refinement (least upper
-- bound) of the set of joints it covers.
genRefinement_ :: MonadRandom m => JointType -> m JointType
genRefinement_ (JT u0 u1) = do
  (_,ss0) <- R.split (UT.toAscList u0)
  (_,ss1) <- R.split (UT.toAscList u1)
  return $ JT (UT.fromDistinctAscList ss0) (UT.fromDistinctAscList ss1)

---------------
-- MUTATIONS --
---------------

data Mutation
  = AddLeft  !Sym !(IntMap Int)
  | AddRight !Sym !(IntMap Int)
  | Add2     !Sym !Sym !Int
  | DelLeft  !Sym !(IntMap Int)
  | DelRight !Sym !(IntMap Int)
  | Del2     !Sym !Sym !Int

-- EVAL --

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
    go (Add2 s0 s1 n01) = deltaDelta_ (nm+n01) dns (vm+2)
      where n0 = ns U.! s0
            n1 = ns U.! s1
            dns = [(n0,n0-n01),(n1,n1-n01)]
    go (DelLeft s0 jtns) = goDel1 s0 jtns
    go (DelRight s1 jtns) = goDel1 s1 jtns
    go (Del2 _ _ n01) = deltaDelta_ (nm-n01) dns (vm-2)
      where dns = [(n01,0),(n01,0)]

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goAdd1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        n01 = sum jtns -- INFO: this could be bookkept
        nm' = nm + n01
        n0 = ns U.! s0
        adns = IM.insertWith (+) s0 n01 jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'-adn))
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'
              -- (because each key of jtns is a staged symbol)
              (IM.insertWith (\_ n0' -> n0') s0 n0 ns') adns
              -- updates on s0 handle case where s0 also appears
              -- (or doesn't) in jtns (i.e. on the other side)

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goDel1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        n01 = sum jtns -- INFO: this could be bookkept
        nm' = nm - n01
        n0 = ns U.! s0
        adns = IM.insertWith (+) s0 n01 jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'+adn))
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'
              (IM.insertWith (\_ n0' -> n0') s0 n0 ns') adns
              -- updates on s0 handle case where s0 also appears
              -- (or doesn't) in jtns (i.e. on the other side)

-- | Computes the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDelta :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> Double
deltaDelta m bigN (nm,nm') dns (vm,vm') =
  nDeltaDelta + sDeltaDelta + rDeltaDelta
  where
    mpN = m + bigN -- m + N
    logFactNpmmnm = logFact $ mpN - nm
    logFactNpmmnm' = logFact $ mpN - nm'
    nDeltaDelta = logFactNpmmnm' - logFactNpmmnm
    -- (logFactNpmm1 and logm cancel out)

    sDeltaDelta = sDDltm + logFact nm - logFact nm'
    sDDltm = sum $ (<$> dns) $ \(ni',ni'') ->
      logFact ni' - logFact ni''
      -- (logFact ni (old symbol count) cancel out)

    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta
    rDeltaDelta = rInfo' - rInfo

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral

-- APPLICATION --

type RefinementT m = StateT (RefinementState m) m

data RefinementState m = RefinementState
  { _model :: !(Model m) -- for params: m, N, ns
  -- , _parent :: !JointType
  , _jointCount :: !Int -- :: nm
  , _refinement :: !JointType -- :: rjt
  , _newSymbolCounts :: !(IntMap Int) -- :: ns'
  , _joints :: !JointsCoverageState }

data JointsCoverageState = JointsCoverageState

  -- {In,Out} x {In,Out} --------------------------------
  { _byFstOutIn   :: !(IntMap (IntMap Int)) -- Out --> In
  , _bySndOutIn   :: !(IntMap (IntMap Int)) -- In <-- Out
  -------------------------------------------------------

  -- (Out,Out) ----------------------------------------------------------------
  , _outSubgraph :: !(Map (Sym,Sym) Int) -- (Out,Out) that can't be added alone

  , _byFstOutOut  :: !(IntMap (IntMap Int)) -- Out --> Out
  , _bySndOutOut  :: !(IntMap (IntMap Int)) -- Out <-- Out
  -----------------------------------------------------------------------------

  -- (In,In) ---------------------------------------------------------------------
  , _byFstInInWithoutDeps :: !(IntMap (IntMap Int)) -- In (w/o dependents) --> In
                                                    -- (can be deleted)

  , _byFstInInWithDeps    :: !(IntMap (IntMap Int)) -- In (>= 1 dependent) --> In
                                                    -- (can't be deleted)

  , _rightDependents      :: !(IntMap (Sym,Int)) -- In (parent) <-- In (dependent)

  , _bySndInInWithoutDeps :: !(IntMap (IntMap Int)) -- In <-- In (w/o dependents)
                                                    -- (can be deleted)

  , _bySndInInWithDeps    :: !(IntMap (IntMap Int)) -- In <-- In (>= 1 dependent)
                                                    -- (can't be deleted)

  , _leftDependents       :: !(IntMap (Sym,Int)) -- In (dependent) --> In (parent)

  , _coDependents :: !(Map (Sym,Sym) Int) } -- (In,In) that can't be deleted alone
  --------------------------------------------------------------------------------

makeLenses ''RefinementState
makeLenses ''JointsCoverageState

-- alternate naming --
addLefts :: Lens' JointsCoverageState (IntMap (IntMap Int))
addLefts = byFstOutIn

addRights :: Lens' JointsCoverageState (IntMap (IntMap Int))
addRights = bySndOutIn

add2s :: Lens' JointsCoverageState (Map (Sym,Sym) Int)
add2s = outSubgraph

delLefts :: Lens' JointsCoverageState (IntMap (IntMap Int))
delLefts = byFstInInWithoutDeps

undelLefts :: Lens' JointsCoverageState (IntMap (IntMap Int))
undelLefts = byFstInInWithDeps

delRights :: Lens' JointsCoverageState (IntMap (IntMap Int))
delRights = bySndInInWithoutDeps

undelRights :: Lens' JointsCoverageState (IntMap (IntMap Int))
undelRights = bySndInInWithDeps

del2s :: Lens' JointsCoverageState (Map (Sym,Sym) Int)
del2s = coDependents
--

enumMutations :: Monad m => RefinementT m [Mutation]
enumMutations = zoom joints $ do
  als <- uses byFstOutIn           $ fmap (uc AddLeft)   . IM.toList
  ars <- uses bySndOutIn           $ fmap (uc AddRight)  . IM.toList
  a2s <- uses outSubgraph          $ fmap (uc (uc Add2)) .  M.toList
  dls <- uses byFstInInWithoutDeps $ fmap (uc DelLeft)   . IM.toList
  drs <- uses bySndInInWithoutDeps $ fmap (uc DelRight)  . IM.toList
  d2s <- uses coDependents         $ fmap (uc (uc Del2)) .  M.toList
  return $ als ++ ars ++ a2s ++ dls ++ drs ++ d2s
  where uc = uncurry

-- APPLICATION --

appMutation :: Monad m => Mutation -> RefinementT m ()
appMutation (AddLeft s0 ins) = do
  zoom joints $ do
    -- (Out,In): remove ({s0} --> ins) (already bound to `ins`)
    byFstOutIn %= IM.delete s0
    --

    -- (Out,Out): remove (and lookup) ({s0} --> outs)
    outs <- byFstOutOut %%= first (fromMaybe IM.empty) . deleteLookup s0
    -- (Out,Out): remove ({s0} <-- outs)
    bySndOutOut %= IM.delete s0 `atEvery` outs
    --

    -- (In,Out): add ({s0} <-- outs) entries into bySndOutIn
    let newSndOutIns = IM.singleton s0 <$> outs
    bySndOutIn %= IM.unionWith (IM.unionWith (err' "bySndOutIn")) newSndOutIns
    --

    -- (In,In)
    exRightDependents <- uses rightDependents (`IM.intersection` ins)
    -- let coParents = fst <$> IM.elems exRightDependents
    -- newDelLefts <- uses byFstInInWithDeps (`IM.restrictKeys` parents)
    forM_ (IM.toList exRightDependents) $ \(exDep1,(coParent0,_)) -> do
      undefined

    undefined

  where
    err' str = error $ "AddLeft (" ++ str ++ "): collision"

    deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
    deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)

    -- apply a function on inner-maps given a set of indexes
    atEvery :: (IntMap a -> IntMap a) -> IntMap b ->
               IntMap (IntMap a) -> IntMap (IntMap a)
    atEvery = flip . IM.differenceWith . flip . const . (nothingIf IM.null .)

    deleteAtEvery :: Sym -> IntMap b -> IntMap (IntMap a) -> IntMap (IntMap a)
    deleteAtEvery s = flip $ IM.differenceWith $
                      \m _ -> nothingIf IM.null $ IM.delete s m



appMutation _ = undefined -- FIXME: TODO

initRefinementState :: PrimMonad m => Model m -> IntMap (IntMap Int) ->
                       IntMap (IntMap Int) -> JointType -> m (RefinementState m)
initRefinementState mdl@(Model _ _ ns _) byFst bySnd rjt = do
  ns' <- IM.traverseWithKey (\s dn -> (+(-dn)) <$> (ns Dyn.! s) ) $
         IM.fromListWith (+) $
         foldr (\((s0,s1),n01) l -> (s0,n01):(s1,n01):l)
         [] byFstInInL

  return $ RefinementState { _model = mdl
                           , _jointCount = nm
                           , _refinement = rjt
                           , _newSymbolCounts = ns'
                           , _joints = coverage }
  where
    coverage = initJointsCoverageState byFst bySnd rjt
    nm = sum $ snd <$> byFstInInL
    byFstInInL = byFstToAscList (coverage ^. byFstInInWithDeps)
                 ++ byFstToAscList (coverage ^. byFstInInWithoutDeps)

initJointsCoverageState :: IntMap (IntMap Int) -> IntMap (IntMap Int) ->
                           JointType -> JointsCoverageState
initJointsCoverageState byFst bySnd (JT u0 u1) =
  JointsCoverageState
  { _byFstOutIn = byFstOutIn_
  , _bySndOutIn = bySndOutIn_

  , _outSubgraph = byFstToMap $
                   (byFstOutOut_ IM.\\ byFstOutIn_)
                   `fmapFilter` (IM.\\ bySndOutIn_)

  , _byFstOutOut = byFstOutOut_
  , _bySndOutOut = bySndOutOut_

  , _byFstInInWithoutDeps = byFstInInWithoutDeps_
  , _byFstInInWithDeps = byFstInInWithDeps_
  , _rightDependents = rightDependents_

  , _bySndInInWithoutDeps = bySndInInWithoutDeps_
  , _bySndInInWithDeps = bySndInInWithDeps_
  , _leftDependents = leftDependents_

  , _coDependents = M.intersectionWith assertEq
                    leftPendantJts rightPendantJts }
  where
    err' = err . ("initJointsCoverageState: " ++)
    assertEq n n'
      | n /= n' = err' $ "should be eq: " ++ show (n,n')
      | otherwise = n

    ( byFstInInWithDeps_
      , byFstInInWithoutDeps_ ) = IM.partition (IM.disjoint rightDependents_)
                                  byFstInIn -- In --> In
    ( bySndInInWithDeps_
      , bySndInInWithoutDeps_ ) = IM.partition (IM.disjoint leftDependents_)
                                  bySndInIn -- In <-- In

    leftDependents_ = IM.mapMaybe fromSingleton byFstInIn
    rightDependents_ = IM.mapMaybe fromSingleton bySndInIn
    fromSingleton im | [sn] <- IM.toList im = Just sn
                     | otherwise = Nothing

    leftPendantJts = M.fromDistinctAscList $ toList leftDependents_
      where toList :: IntMap (s1, a) -> [((Sym, s1), a)]
            toList = IM.foldrWithKey (\s0 s1n l -> first (s0,) s1n : l) []

    rightPendantJts = M.fromDistinctAscList $ toList rightDependents_
      where toList :: IntMap (s0, a) -> [((s0, Sym), a)]
            toList = IM.foldrWithKey (\s1 s0n l -> first (,s1) s0n : l) []

    -- sorting joints --
    byFstIn = byFst `IM.restrictKeys` UT.set u0
    byFstInIn = byFstIn `fmapFilter` (`IM.restrictKeys` UT.set u1)
    byFstInOut = byFstIn `fmapFilter` (`IM.withoutKeys` UT.set u1)

    byFstOut = byFst `IM.withoutKeys` UT.set u0
    byFstOutIn_ = byFstOut `fmapFilter` (`IM.restrictKeys` UT.set u1)
    byFstOutOut_ = byFstOut `fmapFilter` (`IM.withoutKeys` UT.set u1)

    bySndIn = bySnd `IM.restrictKeys` UT.set u1
    bySndInIn = bySndIn `fmapFilter` (`IM.restrictKeys` UT.set u0)
    bySndInOut = bySndIn `fmapFilter` (`IM.withoutKeys` UT.set u0)

    bySndOut = bySnd `IM.withoutKeys` UT.set u1
    bySndOutIn_ = bySndOut `fmapFilter` (`IM.restrictKeys` UT.set u0)
    bySndOutOut_ = bySndOut `fmapFilter` (`IM.withoutKeys` UT.set u0)
    --

-- MAP OPERATIONS --

fmapFilter :: IntMap (IntMap a) -> (IntMap a -> IntMap a) -> IntMap (IntMap a)
fmapFilter = flip $ IM.mapMaybe . (nothingIf IM.null .)

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

stepMutation :: Int -> Int -> U.Vector Int -> RefinementState m -> RefinementState m
stepMutation m bigN ns (RefinementState mdl nm rjt@(JT u0 u1) ns'
                         (JointsCoverageState addLefts_ addRights_
                          add2s_ byFstOutOut_ bySndOutOut_
                          delLefts_ undelLefts_ rightPendants_
                          delRights_ undelRights_ leftPendants_
                          del2s_))
  = seq sortedMuts undefined
  where
    vm = JT.variety rjt

    (minLoss, mut) = head sortedMuts
    sortedMuts = L.sortOn fst $
                 toFst (evalMutation m bigN ns ns' nm vm)
                 <$> allMuts

    allMuts = (uc AddLeft <$> IM.toList addLefts_)
      ++ (uc AddRight     <$> IM.toList addRights_)
      ++ (uc (uc Add2)    <$>  M.toList add2s_)
      ++ (uc DelLeft      <$> IM.toList delLefts_)
      ++ (uc DelRight     <$> IM.toList delRights_)
      ++ (uc (uc Del2)    <$>  M.toList del2s_)
      where uc = uncurry

    -- applying mutations, update maps --
    rjt' = case mut of
      AddLeft s0 in1s -> undefined
        where
          err' str = error $ "AddLeft (" ++ str ++ "): collision"

          addLefts' = IM.delete s0 addLefts_
          addRights' = IM.unionWith (err' "addRights") addRights_ $
                       IM.singleton s0 <$> out1s
          out1s = fromMaybe IM.empty $ -- s0 * Out
                  IM.lookup s0 byFstOutOut_

          add2s' = add2s_ -- no change

          bySndUnorphaneable = IM.intersection rightPendants_ in1s
          noLongerUndelLeft = IS.fromList $
                              fst <$> IM.elems bySndUnorphaneable
          newDelLefts = undelLefts_ `IM.restrictKeys` noLongerUndelLeft
          undelLefts' = undelLefts_ `IM.withoutKeys` noLongerUndelLeft
          delLefts' = IM.insertWith (err' "delLefts_s0") s0 in1s $
                      IM.unionWith (err' "delLefts_undel")
                      newDelLefts delLefts_

          delRights' = delRights_

          del2s' = del2s_ -- no change

      AddRight s1 jtns -> undefined
      Add2 s0 s1 n01 -> undefined
      DelLeft s0 jtns -> undefined
      DelRight s1 jtns -> undefined
      Del2 s0 s1 n01 -> undefined

optimize_ :: Int -> Int -> U.Vector Int ->
             IntMap (IntMap Int) -> IntMap (IntMap Int) -> JointType -> JointType
optimize_ m bigN ns byFst_0 bySnd_0 (JT ru0_0 ru1_0) = undefined
  where
    mpN = m + bigN

    -- from Diagram.Model ------------------------------------
    mLen_ = eliasCodeLen . (+(-256))
    mDelta_ = mLen_ (m+1) - mLen_ m
    tsLen_ = m*m - m - 65280
    tsLen_' = (m+1)*(m+1) - (m+1) - 65280
    tsDelta_ = tsLen_' - tsLen_
    bigNDelta_ k = eliasCodeLen (bigN - k) - eliasCodeLen bigN
    mtnDelta k = mDelta_ + tsDelta_ + bigNDelta_ k
    ----------------------------------------------------------

    logm = ilog m -- log(m)
    logFactNpmm1 = logFact $ mpN - 1 -- log((N + m - 1)!)
    logFactNpmmnm_0 = logFact $ mpN - nm_0 -- log((N + m - nm)!)
    nm_0 = sum $ sum <$> byFstInIn_0

    logFactnm_0 = logFact nm_0

    vm_0 = UT.length ru0_0 + UT.length ru1_0
    rInfo_0 = fromIntegral nm_0 * ilog vm_0

    insideU0_0 = (`IM.restrictKeys` UT.set ru0_0)
    outsideU0_0 = (`IM.withoutKeys` UT.set ru0_0)
    insideU1_0 = (`IM.restrictKeys` UT.set ru1_0)
    outsideU1_0 = (`IM.withoutKeys` UT.set ru1_0)

    -- joints that don't even have a step in the refinement
    unstaged_0 = concatMap (\(s0,s1ns) -> first (s0,) <$> s1ns) $
                 ffmap (IM.toList . outsideU1_0) $
                 IM.toList byFstOut_0

    byFstOut_0 = outsideU0_0 byFst_0

    byFstIn_0 = insideU0_0 byFst_0 -- marginal
    byFstInIn_0 = insideU1_0 <$> byFstIn_0 -- ref.byFst

    -- byFstInOut = outsideU1 <$> byFstIn -- staged

    bySndOut_0 = outsideU1_0 bySnd_0

    bySndIn_0 = insideU1_0 bySnd_0 -- marginal
    bySndInIn_0 = insideU0_0 <$> bySndIn_0 -- ref.bySnd
    -- bySndInOut = outsideU0 <$> bySndIn -- staged

    ss0 = ru0_0 `UT.join` ru1_0
    rns0 = UT.toList ss0

    -- compute the difference in information by the introduction of a
    -- joint type that introduces/changes the following paramters: joint
    -- type count n_m, symbol counts (before,after), and joint type
    -- variety (length u0 + length u1)
    nsrDelta :: Int -> [(Int,Int)] -> Int -> Double
    nsrDelta nm dns vm = nDelta + sDelta + rDelta
      where
        logFactNpmmnm = logFact $ mpN - nm
        nDelta = logFactNpmmnm - logm - logFactNpmm1

        sDelta = sDltm - logFact nm
        sDltm = sum $ (<$> dns) $ \(ni,ni') ->
          logFact ni - logFact ni'

        rDelta = fromIntegral nm * ilog vm

    -- list and evaluate the difference in nsr-loss of introducing
    -- joints whose s1 \in ru1 but s0 \notin ru0 into the refinement
    -- given a map of staged joints
    evalIntro1 :: Int -> IntMap Int -> JointType ->
                  (Int, IntMap Int) -> Double
    evalIntro1 nm rns (JT ru0 ru1) (dnm, s1dns) =
      nDeltaDelta + sDeltaDelta + rDeltaDelta
      where
        -- nDelta' - nDelta
        nDeltaDelta = logFactNpmmnm' - logFactNpmmnm
        -- (logFactNpmm1 and logm cancel out)
        logFactNpmmnm = logFact $ bigN + m - nm
        logFactNpmmnm' = logFact $ bigN + m - nm'
        nm' = nm + dnm -- assert (dnm == sum s1dns)

        -- sDelta' - sDelta
        sDeltaDelta = sDDltm - logFact nm
        sDDltm = sum $ (`IM.mapWithKey` s1dns) $ \s1 drn1 ->
          let n1 = ns U.! s1
              rn1 = rns IM.! s1 -- s1 \in ru1
              n1' = n1 + rn1
              n1'' = n1' + drn1
          in logFact n1' - logFact n1''
          -- (logFact n1 cancel out)

        rDeltaDelta = rDelta' - rDelta
        rDelta' = fromIntegral nm' * ilog vm'
        rDelta = fromIntegral nm * ilog vm
        vm = UT.length ru0 + UT.length ru1
        vm' = vm + 1 -- one symbol added: s0
