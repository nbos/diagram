{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrIntervals (module Diagram.ConstrIntervals) where

import Debug.Trace
import GHC.Utils.Monad
import GHC.Show

import Control.Monad hiding (join)
import Control.Lens hiding (Index,(:>))
import Control.Monad.State.Strict

import Data.Maybe
import Data.Bifunctor
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Streaming (Of(..), Stream)
import qualified Streaming.Prelude as S

import Diagram.String
import Diagram.Primitive

import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT
import qualified Diagram.UnionType as UT

import Diagram.ConstrInterval (CI(..))
import qualified Diagram.ConstrInterval as CI

import Diagram.Util

-- | Constructive Intervals. Joint concordance\/index. All construction
-- sites in corpus, as intervals, for fast join\/union but slower
-- delete\/subtract (have first to resolve each subtracted interval into
-- its super-interval).
data CIs = CIs
  { _jointType :: !JointType      -- :: (u0, u1)
  , _symCounts :: !(IntMap Count) -- :: s  --> n
  , _byHead    :: !(IntMap CI)    -- :: hd --> (hd, shd, len, tl, stl)
  , _byTail    :: !(IntMap CI) }  -- :: tl --> (hd, shd, len, tl, stl)
  deriving(Eq) -- TODO: joint count?
makeLenses ''CIs

instance Show CIs where
  showsPrec :: Int -> CIs -> ShowS
  showsPrec p cis@(CIs jt ns bhd btl)
    | cis == empty = showString "empty"
    | otherwise = showParen (p >= 11) $
                  showString "CIs {"
                  . showString "_jointType = " . showsPrec 0 jt
                  . showCommaSpace
                  . showString "_symCounts = " . showsPrec 0 ns
                  . showCommaSpace
                  . showString "_byHead = " . showsPrec 0 bhd
                  . showCommaSpace
                  . showString "_byTail = " . showsPrec 0 btl . showString "}"

toList :: CIs -> [CI]
toList = IM.elems . _byHead

-- | Count the number of constructions
jointCount :: CIs -> Int
jointCount (CIs _ ns _ _) | odd sum_ns = error ""
                          | otherwise = sum_ns `div` 2
  where sum_ns = sum ns

-- | Delete a CI which is part of set from the set. Symbol counts will
-- be messed up if the CI is not member of the of set. Doesn't modify
-- the joint type.
deleteExisting :: PrimMonad m => Doubly (PrimState m) -> CI -> CIs -> m CIs
deleteExisting _ (CI hd shd 2 tl stl) (CIs jt ns bhd btl) =
  pure CIs{ _jointType = jt
          , _symCounts = IM.update (nothingIf (== 0) . (+(-1))) shd $
                         IM.update (nothingIf (== 0) . (+(-1))) stl ns
          , _byHead = IM.delete hd bhd
          , _byTail = IM.delete tl btl }
deleteExisting dly ci@(CI hd _ _ tl _) (CIs jt ns bhd btl) = do
  ndns <- CI.symCounts dly ci
  return $ CIs{ _jointType = jt
              , _symCounts = flip2 IM.differenceWith ns ndns $
                             nothingIf (== 0) .: (-)
              , _byHead = IM.delete hd bhd
              , _byTail = IM.delete tl btl }

-- | Insert a CI which is not present, nor overlapping with any interval
-- (including point contact, or end-to-end-touching) in the
-- set. Invariants break if the condition doesn't hold. Doesn't modify
-- the joint type.
insertDisjoint :: PrimMonad m => Doubly (PrimState m) -> CI -> CIs -> m CIs
insertDisjoint _ ci@(CI hd shd 2 tl stl) (CIs jt ns bhd btl) =
  pure CIs{ _jointType = jt
          , _symCounts = IM.insertWith (+) shd 1 $
                         IM.insertWith (+) stl 1 ns
          , _byHead = IM.insert hd ci bhd
          , _byTail = IM.insert tl ci btl }
insertDisjoint dly ci@(CI hd _ _ tl _) (CIs jt ns bhd btl) = do
  dns <- CI.symCounts dly ci
  return $ CIs{ _jointType = jt
              , _symCounts = IM.unionWith (+) dns ns
              , _byHead = IM.insert hd ci bhd
              , _byTail = IM.insert tl ci btl }

err :: String -> a
err = error . ("ConstrIntervals." ++)

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
        . (byHead %~ IM.insertWithKey err' i0 ci)
        . (byTail %~ IM.insertWithKey err' i1 ci)

    | otherwise -> do -- s0 == s1
        is :> ss'' <- S.toList $ S.map fst $ S.span ((s0 ==) . snd) ss'
        let len = length is + 2
            itl = last $ i1:is
            constrlen = (len `div` 2) * 2
            empty' = empty{ _jointType = JT.singleton s0 s0 }
            ci = CI i0 s0 len itl s0

        fromStream_0 (itl,s0) ss'' $ m & at (s0,s0) . non empty' %~
          (symCounts %~ IM.insertWith (+) s0 constrlen)
          . (byHead %~ IM.insertWithKey err' i0 ci)
          . (byTail %~ IM.insertWithKey err' itl ci)

  where
    err' :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err' = err . ("fromStream: collision: " ++) . show .:. (,,)

-----------------
-- COMPOSITION --
-----------------

data JoinState = JoinState
  { __A :: !CIs -- some set of intervals 'A'
  , __B :: !CIs -- some other set of intervals 'B'
  , _joinCor :: !(IntMap Int) -- sym count delta accumulator
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
join_ ciAs ciBs = flip evalState (JoinState ciAs ciBs IM.empty) $ do

  unless (uB0 `UT.disjoint` uA1) $ -- short-circuit intersection (O(m) <<< O(N))
    uses2 (_B.byHead) (_A.byTail) IM.intersection
    >>= mapM_ go

  unless (uA0 `UT.disjoint` uB1) $ do -- short-circuit intersection (O(m) <<< O(N))
    cols <- uses2 (_A.byHead) (_B.byTail) IM.intersection
    modify $ \(JoinState a b d) -> JoinState b a d -- A <--> B
    mapM_ go cols
    modify $ \(JoinState b a d) -> JoinState a b d -- B <--> A

  -- fold new sym count deltas into the counts map
  cor <- use joinCor
  bhd <- uses2 (_A.byHead) (_B.byHead) $ IM.unionWithKey err'
  btl <- uses2 (_A.byTail) (_B.byTail) $ IM.unionWithKey err'
  let ns' = -- L.foldl' (flip $ uc alter) ns (IM.toList dns)
        IM.mergeWithKey (\_ n dn -> nothingIf (==0) (n + dn))
        id id ns cor

  return (CIs jt ns' bhd btl, cor)

  where
    JT uA0 uA1 = ciAs^.jointType
    JT uB0 uB1 = ciBs^.jointType
    jt = JT.join (ciAs^.jointType) (ciBs^.jointType)
    ns = IM.unionWith (+) (ciAs^.symCounts) (ciBs^.symCounts)

    alter s d = flip IM.alter s $ \case
      Nothing -> Just d
      Just n -> nothingIf (== 0) (n + d)

    -- inc = inc_ 1
    dec = inc_ (-1)
    inc_ :: Int -> Sym -> StateT JoinState Identity ()
    inc_ d s = joinCor %= alter s d

    -- | Given a constructive interval from 'B' whose head collides with
    -- the tail of an interval of the other set 'A', join together, fix
    -- count if required and re-insert in 'A' set
    go :: CI -> StateT JoinState Identity ()
    go ciB@(CI tlA@hdB _ lenB tlB stlB) = do
      -- can't accept a ciA independent of any previous go calls (as in
      -- IM.intersectionWith (,)) because it could have been pre-pended
      -- to (sandwich case), so we lookup (NOTE: doesn't that only
      -- happen on the second pass though?)
      ciA@(CI hdA _ lenA _ stlA) <-
        _A.byTail %%= first fromJust . deleteLookup tlA -- delete [.. tlA]

      _B.byHead %= IM.delete hdB -- delete [hdB ..]
      _B.byTail %= IM.delete tlB -- delete [.. tlB]

      when (even lenA) $ dec stlA
      let ciAB@(CI _ _ lenAB _ _) = CI.unsafeJoin ciA ciB

      ((_A.byHead) %%= deleteLookup tlB >>=) $ \case
        -- simple collision: [hdA..tlA) <> [hdB..tlB] ==> [hdA..tlB]
        Nothing -> do
          _A.byHead %= IM.insert hdA ciAB -- update hdA
          _A.byTail %= IM.insert tlB ciAB -- insert tlB
          let d = fromEnum (even lenAB) - fromEnum (even lenB)
          unless (d == 0) $ inc_ d stlB

        -- sandwich: [hdA..tlA) <> [hdB..tlB) <> [hdA2..tlA2] ==> [hdA..tlA2]
        Just ciA2@(CI hdA2 _ lenA2 tlA2 stlA2) -> do
          when (even lenB) $ dec stlB
          let ciABA@(CI _ _ lenABA _ _) = CI.unsafeJoin ciAB ciA2
          _A.byHead %= IM.delete hdA2 -- delete hdA2
          _A.byHead %= IM.insert hdA ciABA -- update hdA
          _A.byTail %= IM.insert tlA2 ciABA -- update tlA2
          let d = fromEnum (even lenABA) - fromEnum (even lenA2)
          unless (d == 0) $ inc_ d stlA2

    err' :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err' = error . ("join: collision: " ++) . show .:. (,,)

deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)
{-# INLINE deleteLookup #-}

----------------
-- DIFFERENCE --
----------------

data DiffState = DiffState
  { _byHeadRes :: IntMap CI
  , _byTailRes :: IntMap CI
  , _diffCor :: !(IntMap Int) -- sym count delta accumulator
} deriving (Show,Eq)
makeLenses ''DiffState

-- | Remove the constructions of the second set of CIs from the first,
-- given a reference string and optionally membership functions for the
-- super (left) and sub (right) operands. Cannot determine if the joints
-- of the sub set are sufficient to eliminate any symbol of the joint
-- type of the super type without a complete scan of the super's joints,
-- so the type field is left untouched. Also returns (snd) the
-- correction on the counts between the difference in the counts and the
-- counts of the difference (i.e. the Cor of this mut).
difference :: PrimMonad m => Doubly (PrimState m) ->
  Maybe (Sym -> Sym -> m Bool) -> Maybe (Sym -> Sym -> m Bool) ->
  CIs -> CIs -> m (CIs, IntMap Int)
difference dly memSuper_ memSub_ cisA (CIs jtB _ bhdB _) = do
  DiffState bhdA' btlA' dnsA <- flip execStateT (DiffState bhdA btlA IM.empty) $
    forM_ (IM.elems bhdB) $ \ci@(CI subHd _ _ subTl _) -> (getSuper ci >>=) $ \case
      Nothing -> do -- super doesn't start here, but ci is inside it
        ((diffCor %=) . imUnion) =<< CI.symCounts dly ci
      Just Nothing -> do -- super is identical, remove indexes, cor is 0
        byHeadRes %= IM.delete subHd
        byTailRes %= IM.delete subTl
      Just (Just (super@(CI superHd _ _ superTl _), rems)) -> do -- case super
        byHeadRes %= IM.delete superHd
        byTailRes %= IM.delete superTl
        ciCounts <- CI.symCounts dly ci
        supCounts <- CI.symCounts dly super
        diffCor %= imUnion ((negate <$> ciCounts) `imUnion` supCounts)
        forM_ rems $ \r@(CI remHd _ _ remTl _) -> do
          byHeadRes %= IM.insert remHd r
          byTailRes %= IM.insert remTl r
          remCounts <- CI.symCounts dly r
          diffCor %= imUnion (negate <$> remCounts)

  let nsA' = imUnion nsA dnsA
  return (CIs jtA nsA' bhdA' btlA', dnsA)

  where
    CIs jtA nsA bhdA btlA = cisA
    memSuper = fromMaybe (return .: JT.member jtA) memSuper_
    memSub   = fromMaybe (return .: JT.member jtB) memSub_
    getSuper = lift . CI.superCI dly memSuper memSub

-----------
-- DEBUG --
-----------

checkIntegrityM :: PrimMonad m => Doubly (PrimState m) -> CIs -> m ()
checkIntegrityM dly (CIs jt ns bhd btl)
  | odd (sum ns) = err' $ "sum of counts is odd: " ++ show ns
  | bhdCIs /= btlCIs =
      err' $ "byHead and byTail don't contain the same CI's: " ++ show (bhd,btl)
  | otherwise = do
      ijts <- concat <$> mapM (CI.jointExtension dly) cisL
      let (s0s, s1s) = unzip $ snd <$> ijts
          jt_verif = JT.fromLists s0s s1s
      when (jt /= jt_verif) $
        err' $ "type doesn't correspond to member joints: " ++ show (jt, cisL)
      --
      ns_verif <- IM.unionsWith (+) <$> mapM (CI.symCounts dly) cisL
      when (ns /= ns_verif) $
        err' $ "sym counts don't match extension's: " ++ show (ns, ns_verif)
  where
    err' = err . ("checkIntegrity: " ++)
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs

-- | Pure version of checkIntegrityM, almost as good
checkIntegrity :: CIs -> a -> a
checkIntegrity (CIs jt ns bhd btl)
  | odd (sum ns) = err' $ "sum of counts is odd: " ++ show ns
  | bhdCIs /= btlCIs =
      err' $ "byHead and byTail don't contain the same CI's: " ++ show (bhd,btl)
  | not (and (IM.intersectionWith (<=) minCounts ns)) =
      err' $ "recorded symbol counts impossibly low: "
      ++ show ((cisL,ns),minCounts)
  | otherwise = foldr (.) id $ (<$> cisL) $ \ci@(CI _ shd _ _ stl) ->
      if JT.member jt shd stl then id
      else err' $ "edges of interval " ++ show ci
           ++ " don't fit in type " ++ show jt
  where
    err' = err . ("checkIntegrity: " ++)
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs
    minCounts = L.foldl' (\m s -> IM.insertWith (+) s (1::Int) m) IM.empty $
                foldr (\(CI _ shd _ _ stl) l -> shd:stl:l) [] cisL

-- | Errorless version of @checkIntegrity@
valid :: PrimMonad m => Doubly (PrimState m) -> CIs -> m Bool
valid dly (CIs jt ns bhd btl) = do
  ijts <- concat <$> mapM (CI.jointExtension dly) cisL
  let (s0s, s1s) = unzip $ snd <$> ijts
      jt_verif = JT.fromLists s0s s1s
  ns_verif <- unions <$> mapM (CI.symCounts dly) cisL
  return $ even (sum ns)
    && bhdCIs == btlCIs && jt == jt_verif && ns == ns_verif
  where
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs
    unions = fromMaybe IM.empty . foldTree (IM.unionWith (+))

debug_join :: PrimMonad m => Doubly (PrimState m) -> CIs -> CIs -> m CIs
debug_join = fmap fst .:. debug_join_

debug_join_ :: PrimMonad m => Doubly (PrimState m) ->
               CIs -> CIs -> m (CIs, IntMap Int)
debug_join_ dly cisA cisB = do
  unlessM (valid dly cisA) $ do
    traceM' $ "supplied CIs not valid (left): \n" ++ show cisA
    checkIntegrityM dly cisA
  unlessM (valid dly cisB) $ do
    traceM' $ "supplied CIs not valid (right): \n" ++ show cisB
    checkIntegrityM dly cisB
  unlessM (valid dly cisC) $ do
    traceM' $ "join CIs not valid. \n\n"
      ++ "left: " ++ show cisA ++ "\n\n"
      ++ "right: " ++ show cisB ++ "\n\n"
      ++ "join: " ++ show cisC ++ "\n"
    checkIntegrityM dly cisC
  -- traceM' $ "CIs join OK: " ++ pShow (toList cisC)
  return res
  where
    res@(cisC,_) = join_ cisA cisB
    traceM' = traceM . ("ConstrIntervals.debug_join_: " ++)
