{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TupleSections, LambdaCase, TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}

module Diagram.JointType (module Diagram.JointType, Sym) where

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

import qualified Codec.Arithmetic.Variety as Variety
import Codec.Arithmetic.Variety.BitVec (BitVec)
import qualified Codec.Arithmetic.Variety.BitVec as BV

import Diagram.Primitive
import Diagram.Joints (Joints)
import Diagram.UnionType (Sym,UnionType(..))
import qualified Diagram.UnionType as UT
import qualified Diagram.Random as R
import Diagram.Information

import Diagram.Util

-- | A joint of unions
data JointType = JT {
  left :: !UnionType, -- s0s
  right :: !UnionType -- s1s
} deriving (Eq)

instance Show JointType where
  show :: JointType -> String
  show = ("fromLists " ++) . show . toLists

size :: JointType -> (Int, Int)
size (JT u0 u1) = (UT.length u0, UT.length u1)

fromJoints :: Joints -> JointType
fromJoints = uncurry JT . both UT.fromList . unzip . M.keys

fromLists :: ([Sym],[Sym]) -> JointType
fromLists (syms0, syms1) = JT (UT.fromList syms0) (UT.fromList syms1)

toLists :: JointType -> ([Sym],[Sym])
toLists (JT u0 u1) = (UT.toAscList u0, UT.toAscList u1)

-- | Is the symbol a member of the union?
member :: (Sym,Sym) -> JointType -> Bool
member (s0,s1) (JT u0 u1) = UT.member s0 u0 && UT.member s1 u1

-------------
-- LATTICE --
-------------

-- | Bottom (empty) type
bot :: JointType
bot = JT UT.bot UT.bot

-- | Subtype relation (partial order)
leq :: JointType -> JointType -> Bool
leq (JT u0 u1) (JT u0' u1') =
  UT.length u1 <= UT.length u1' -- short-circuit
  && UT.leq u0 u0'
  && UT.leq u1 u1'

-- | Strict subtype relation (partial order)
lt :: JointType -> JointType -> Bool
lt t t' = leq t t' && t /= t'

-- | Least upper bound
join :: JointType -> JointType -> JointType
join (JT u0 u1) (JT u0' u1') =
  JT (UT.join u0 u0') (UT.join u1 u1')

-- | Greatest lower bound
meet :: JointType -> JointType -> JointType
meet (JT u0 u1) (JT u0' u1') =
  JT (UT.meet u0 u0') (UT.meet u1 u1')

-----------
-- CODEC --
-----------

-- | Refine a type with a bit mask
refine :: JointType -> BitVec -> JointType
refine (JT u0 u1) bv
  | len /= n0 + n1 = err $ "refine: bitvec length mismatch: "
                     ++ show len ++ " should be " ++ show (n0 + n1)
  | otherwise = JT (UT.refine u0 bv0) (UT.refine u1 bv1)
  where
    n0 = UT.length u0
    n1 = UT.length u1
    len = BV.length bv
    (bv0,bv1) = BV.splitAt n0 bv

-- | For a joint count, a type and a code, instantiate the type into
-- specific joints. Returns Nothing if the given code is not long enough
-- to fully specify a resolution. If code is long enough, returns the
-- rest of the code with the resolution's code removed.
resolve :: Int -> JointType -> BitVec -> Maybe ([(Sym,Sym)], BitVec)
resolve k t bv = first (resolveIndexes t) <$> Variety.decode (bases k t) bv

-- | For encoding/decoding, [n0,n1,n0,n1,n0,n1,...]
bases :: Int -> JointType -> [Integer]
bases k (JT u0 u1) = concat $
  L.transpose [UT.bases k u0, UT.bases k u1]

-- | For decoding
resolveIndexes :: JointType -> [Integer] -> [(Sym,Sym)]
resolveIndexes (JT u0 u1) is = zip s0s s1s
  where
    s0s = UT.fromIdxs u0 is0
    s1s = UT.fromIdxs u1 is1
    (is0,is1) = case L.transpose (L.chunksOf 2 is) of
                  [l0,l1] -> (l0,l1)
                  _else -> err "resolveIndexes: impossible"

err :: String -> a
err = error . ("JointType." ++)

-----------------
-- INFORMATION --
-----------------

-- | Codelen (bits) of a refinement
refineLen :: JointType -> Int
refineLen (JT u0 u1) = UT.refineLen u0 + UT.refineLen u1

-- | Number of different joints covered by the type
variety :: JointType -> Int
variety (JT u0 u1) = UT.length u0 * UT.length u1

-- | k log(n0*n1)
resolutionInfo :: Int -> JointType -> Double
resolutionInfo k jt = fromIntegral k * log2 base
  where base = fromIntegral $ variety jt

-- | Length of a code to instantiate a type into `k` constructions
resolutionLen :: Int -> JointType -> Int
resolutionLen = ceiling .: resolutionInfo

---------------------------
-- REFINEMENT GENERATION --
---------------------------

-- | O(n^2) Generate all combinations
comb :: [a] -> [[a]]
comb [] = [[]]
comb (a:as) = ass ++ fmap (a:) ass
  where ass = comb as

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
    u0s = comb $ M.toAscList byFst0 -- deconstruct, select

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
data RefinementSamplingState a = RefinementSamplingState {
  -- NOTE: Map Int instead of IntMap because we want O(1) size
  -- and O(log n) elemAt.
  _jtsByFst :: !(Map Sym ([Sym], Map Sym a)),
  _jtsBySnd :: !(Map Sym ([Sym], Map Sym a)),
  _fstUnion :: !IntSet,
  _sndUnion :: !IntSet,
  _refJoints :: !(Map (Sym,Sym) ())
}
makeLenses ''RefinementSamplingState

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
  evalStateT go $ RefinementSamplingState
  (([],) <$> byFst0)
  (([],) <$> bySnd0) IS.empty IS.empty M.empty
  where
    go :: StateT (RefinementSamplingState a) m (JointType, Map (Sym,Sym) ())
    go = get >>= \case
      (RefinementSamplingState byFst bySnd u0 u1 ref)
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
    goElimFst :: Bool -> Int -> StateT (RefinementSamplingState a) m ()
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
    goElimSnd :: Bool -> Int -> StateT (RefinementSamplingState a) m ()
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

-------------------------
-- REFINEMENT MUTATION --
-------------------------

data RefinementMut
  = AddLeft  !Sym !(IntMap Int)
  | AddRight !Sym !(IntMap Int)
  | Add2     !Sym !Sym !Int
  | DelLeft  !Sym !(IntMap Int)
  | DelRight !Sym !(IntMap Int)
  | Del2     !Sym !Sym !Int

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral

data RefinementMutationState = RefinementMutationState
  { _newSymCounts  :: !(IntMap Int)
  , _jointCount    :: !Int

  , _addLefts      :: !(IntMap (IntMap Int))
  , _addRights     :: !(IntMap (IntMap Int))
  , _add2s         :: !(Map (Sym,Sym) Int)

  , _byFstOutOut   :: !(IntMap (IntMap Int))
  , _bySndOutOut   :: !(IntMap (IntMap Int))

  , _delLefts      :: !(IntMap (IntMap Int))
  , _undelLefts    :: !(IntMap (IntMap Int))
  , _rightPendants :: !(IntMap (Sym,Int))

  , _delRights     :: !(IntMap (IntMap Int))
  , _undelRights   :: !(IntMap (IntMap Int))
  , _leftPendants  :: !(IntMap (Sym,Int))

  , _del2s         :: !(Map (Sym,Sym) Int)
  , _refinement    :: !JointType }
makeLenses ''RefinementMutationState

initRefinementMutationState :: U.Vector Int -> IntMap (IntMap Int) ->
  IntMap (IntMap Int) -> JointType -> RefinementMutationState
initRefinementMutationState ns byFst bySnd rjt@(JT u0 u1) =

  RefinementMutationState
  { _newSymCounts = ns'
  , _jointCount = nm

  , _addLefts = byFstOutIn -- Out --> In
  , _addRights = bySndOutIn -- In <-- Out
  , _add2s = byFstToMap $ -- (Out,Out) that are not addable individually
             (byFstOutOutVar IM.\\ byFstOutIn) `fmapFilter` (IM.\\ bySndOutIn)

  , _byFstOutOut = byFstOutOutVar
  , _bySndOutOut = bySndOutOutVar

  , _delLefts = delLefts_ -- In --> In (no dependents)
  , _undelLefts = undelLefts_ -- In --> In (have >= 1 dependent)
  , _rightPendants = rightDependents

  , _delRights = delRights_ -- In <-- In (no dependents)
  , _undelRights = undelRights_ -- In <-- In (have >= 1 dependent)
  , _leftPendants = leftDependents

  , _del2s = M.intersectionWith assertEq -- (In,In) co-dependent
             leftPendantJts rightPendantJts
  , _refinement = rjt } -- res

  where
    ns' = IM.mapWithKey (\s dn -> (ns U.! s) - dn) $
          IM.fromListWith (+) $
          foldr (\((s0,s1),n01) l -> (s0,n01):(s1,n01):l)
          [] byFstInInL

    nm = sum $ snd <$> byFstInInL
    byFstInInL = byFstToAscList byFstInIn

    err' = err . ("initRefinementMutationState: " ++)
    assertEq n n'
      | n /= n' = err' $ "should be eq: " ++ show (n,n')
      | otherwise = n

    (undelLefts_, delLefts_) = IM.disjoint rightDependents
                               `IM.partition` byFstInIn
    (undelRights_, delRights_) = IM.disjoint leftDependents
                                 `IM.partition` bySndInIn

    -- (de)pendant symbols --> (dependor, count)
    leftDependents = IM.mapMaybe fromSingleton byFstInIn
    rightDependents = IM.mapMaybe fromSingleton bySndInIn
    fromSingleton im | [sn] <- IM.toList im = Just sn
                     | otherwise = Nothing

    leftPendantJts = M.fromDistinctAscList $
      byFstSingletonToAscList leftDependents
    rightPendantJts = M.fromDistinctAscList $
      bySndSingletonToAscList rightDependents

    -- initial indexing --
    byFstIn = byFst `IM.restrictKeys` UT.set u0
    byFstInIn = byFstIn `fmapFilter` (`IM.restrictKeys` UT.set u1)
    byFstInOut = byFstIn `fmapFilter` (`IM.withoutKeys` UT.set u1)

    byFstOut = byFst `IM.withoutKeys` UT.set u0
    byFstOutIn = byFstOut `fmapFilter` (`IM.restrictKeys` UT.set u1)
    byFstOutOutVar = byFstOut `fmapFilter` (`IM.withoutKeys` UT.set u1)

    bySndIn = bySnd `IM.restrictKeys` UT.set u1
    bySndInIn = bySndIn `fmapFilter` (`IM.restrictKeys` UT.set u0)
    bySndInOut = bySndIn `fmapFilter` (`IM.withoutKeys` UT.set u0)

    bySndOut = bySnd `IM.withoutKeys` UT.set u1
    bySndOutIn = bySndOut `fmapFilter` (`IM.restrictKeys` UT.set u0)
    bySndOutOutVar = bySndOut `fmapFilter` (`IM.withoutKeys` UT.set u0)
    --

byFstSingletonToAscList :: IntMap (s1, a) -> [((Sym, s1), a)]
byFstSingletonToAscList =
  IM.foldrWithKey (\s0 s1n l -> first (s0,) s1n : l) []

bySndSingletonToAscList :: IntMap (s0, a) -> [((s0, Sym), a)]
bySndSingletonToAscList =
  IM.foldrWithKey (\s1 s0n l -> first (,s1) s0n : l) []

fmapFilter :: IntMap (IntMap a) -> (IntMap a -> IntMap a) ->
              IntMap (IntMap a)
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

optimize :: Int -> Int -> U.Vector Int -> RefinementMutationState
  -> RefinementMutationState
optimize m bigN ns (RefinementMutationState ns' nm addLeftsVar addRightsVar
                    add2sVar byFstOutOutVar bySndOutOutVar
                    delLeftsVar undelLeftsVar rightPendantsVar
                    delRightsVar undelRightsVar leftPendantsVar
                    del2sVar rjt@(JT u0 u1))
  = seq sortedMuts undefined
  where
    vm = variety rjt

    (minLoss, mut) = head sortedMuts
    sortedMuts = L.sortOn fst $
                 toFst (evalMut m bigN ns ns' nm vm)
                 <$> allMuts

    allMuts = (uc AddLeft <$> IM.toList addLeftsVar)
      ++ (uc AddRight     <$> IM.toList addRightsVar)
      ++ (uc (uc Add2)    <$>  M.toList add2sVar)
      ++ (uc DelLeft      <$> IM.toList delLeftsVar)
      ++ (uc DelRight     <$> IM.toList delRightsVar)
      ++ (uc (uc Del2)    <$>  M.toList del2sVar)
      where uc = uncurry

    -- applying mutations, update maps --
    rjt' = case mut of
      AddLeft s0 in1s -> undefined
        where
          err str = error $ "AddLeft (" ++ str ++ "): collision"

          addLefts' = IM.delete s0 addLeftsVar
          addRights' = IM.unionWith (err "addRights") addRightsVar $
                       IM.singleton s0 <$> out1s
          out1s = fromMaybe IM.empty $ -- s0 * Out
                  IM.lookup s0 byFstOutOutVar

          add2s' = add2sVar -- no change

          bySndUnorphaneable = IM.intersection rightPendantsVar in1s
          noLongerUndelLeft = IS.fromList $
                              fst <$> IM.elems bySndUnorphaneable
          newDelLefts = undelLeftsVar `IM.restrictKeys` noLongerUndelLeft
          undelLefts' = undelLeftsVar `IM.withoutKeys` noLongerUndelLeft
          delLefts' = IM.insertWith (err "delLefts_s0") s0 in1s $
                      IM.unionWith (err "delLefts_undel")
                      newDelLefts delLeftsVar

          delRights' = delRightsVar

          del2s' = del2sVar -- no change

      AddRight s1 jtns -> undefined
      Add2 s0 s1 n01 -> undefined
      DelLeft s0 jtns -> undefined
      DelRight s1 jtns -> undefined
      Del2 s0 s1 n01 -> undefined


evalMut :: Int -> Int -> U.Vector Int -> IntMap Int -> Int -> Int ->
           RefinementMut -> Double
evalMut m bigN ns ns' nm vm = go
  where
    dd :: Int -> [(Int,Int)] -> Int -> Double
    dd nm' dns vm' = deltaDelta m bigN (nm,nm') dns (vm,vm')

    go :: RefinementMut -> Double
    go (AddLeft s0 jtns) = goAdd1 s0 jtns
    go (AddRight s1 jtns) = goAdd1 s1 jtns
    go (Add2 s0 s1 n01) = dd (nm+n01) dns (vm+2)
      where n0 = ns U.! s0
            n1 = ns U.! s1
            dns = [(n0,n0-n01),(n1,n1-n01)]
    go (DelLeft s0 jtns) = goDel1 s0 jtns
    go (DelRight s1 jtns) = goDel1 s1 jtns
    go (Del2 _ _ n01) = dd (nm-n01) dns (vm-2)
      where dns = [(n01,0),(n01,0)]

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goAdd1 s0 jtns = dd nm' (IM.elems dns) (vm+1)
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
    goDel1 s0 jtns = dd nm' (IM.elems dns) (vm+1)
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

-- compute the difference in the info delta from changing
-- parameters: joint type count n_m (before,after), symbol (new)
-- counts ns (before,after), and joint type variety (before,after)
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
