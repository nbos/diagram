{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

module Diagram.Evolution.Correction (module Diagram.Evolution.Correction) where

import Debug.Trace

import Prelude hiding (init)

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Primitive
import Diagram.String

import qualified Diagram.Doubly as D
import Diagram.ConstrInterval(CI(..))
import qualified Diagram.ConstrInterval as CI

import Diagram.Evolution.Mutation (Mutation(..))
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS

import Diagram.Util

-- Here we compute, for constructive intervals (CIs) and mutations, the
-- difference between the symbol counts of the union/join of the CIs &
-- those of the mutation's (i.e. (cis U mut.cis).ns) and the sum of the
-- symbol counts of the CIs and those of the mutation's (i.e. (cis.ns +
-- mut.cis.ns)).

---------
-- Cor --
---------

type Cor = Map Mutation (IntMap Int)

empty :: Cor
empty = M.empty

-- | Instead of M.unionWith (IM.unionWith (+)) and having to worry about
-- empty\/null entries
union :: Cor -> Cor -> Cor
union = M.mergeWithKey (const f) id id
  where
    f :: IntMap Int -> IntMap Int -> Maybe (IntMap Int)
    f = nothingIf IM.null .: IM.mergeWithKey (const g) id id
    g :: Int -> Int -> Maybe Int
    g = nothingIf (==0) .: (+)

unions :: [Cor] -> Cor
unions [] = empty
unions (c:cs) = L.foldl' union c cs -- (use foldTree?)

-----------------
-- ENTRY POINT --
-----------------

onAllMuts :: PrimMonad m =>
  Doubly (PrimState m) -> TypeState (PrimState m) -> CI -> m Cor
onAllMuts dly tst ci = do
  onAdd <- onAddMuts dly tst ci
  onDel <- onDelMuts dly tst ci
  return $ M.unionWith (error "impossible") onAdd onDel

----------------------
-- ON ADD MUTATIONS --
----------------------

-- | Corrections on add-muts regarding all chains of a given CI,
-- injective, with no `sub` predicate.
onAddMuts :: PrimMonad m =>
  Doubly (PrimState m) -> TypeState (PrimState m) -> CI -> m Cor
onAddMuts dly tst ci = do
  traceM ""
  traceM ("[ADD COR]: " ++ show ci)
  res <- f dly tst ci
  traceShowM res
  return res
  where
    f = fmap (unions . fmap (uc onAddMuts_)) .:. composeAdds

-- WHERE --

-- | Given a non-empty list of overlapping/connecting intervals brought
-- together by an Add-mutation, alternating [in-]add-in-add-etc., return
-- the appropriate correction on the mut's CIs' sym counts as a
-- singleton of the given mutation, which is assumed to be the
-- Add-mutation bringing together the sequence of intervals, although
-- this is not checked. Correction values are signed in order to be
-- added to the mut's CIs' counts *before* they are added/subtracted
-- from the type's or string's counts.
onAddMuts_ :: Mutation -> NonEmpty CI -> Cor
onAddMuts_ mut cis | IM.null cor = M.empty
                   | otherwise   = M.singleton mut cor
  where
    cor = IM.filter (/= 0) $ flip execState IM.empty $ do
      forM_ (NE.init cis) $ \(CI _ _ len _ stl) ->
        when (even len) $ modify $ IM.insertWith (+) stl (-1)
      let CI _ _ oldLen _ tailSym = NE.last cis
          newLen = sum (_ciLength <$> cis) -- constituents lengths
                   - (length cis - 1) -- overlaps
          d = fromEnum (even newLen) - fromEnum (even oldLen)
      when (d /= 0) $ modify $ IM.insertWith (+) tailSym d

-- TODO: factor/optimize case order
-- | Grab the maximal [in-]add-in-add-etc. chains surrounding a given
-- in-CI, both the left and right ones (if they differ in their
-- mutation, otherwise they are returned as one) if there are no
-- in-intervals to the left of the given in-CI which have a joint not
-- satisfying the `sub` predicate (basically, if there is a non-sub
-- interval to the left, the chain is cancelled, to be canonically
-- returned by the leftmost non-sub in-CI when applied to this
-- function).
--
-- Also returns (snd) all sub-intervals which don't connect to/touch a
-- non-sub interval satisfying the given `sub` predicate (both to the
-- left and right), nor those that are contained within the given in-CI.
composeAddsSub :: PrimMonad m => (Sym -> Sym -> Bool) ->
                  Doubly (PrimState m) -> TypeState (PrimState m) ->
                  CI -> m (Maybe [(Mutation, NonEmpty CI)], [CI])
composeAddsSub sub dly tst ci = (<$> liftA2 (,) prevCIs nextCIs) $ \case
  (Nothing, Nothing) -> (Just [], [])
  (Nothing, Just (addMut, nexts, rsubs)) -> (Just [(addMut, ci <| nexts)], rsubs)
  (Just (_, Nothing), Nothing) -> (Just [], [])
  (Just (addMut, Just (prvs, lsubs)), Nothing)
    | null lsubs -> (Just [(addMut, prvs <> (ci:|[]))], [])
    | otherwise  -> (Nothing, lsubs) -- escalate non canon
  (Just (addMut, Nothing), Just (addMut', nexts, rsubs))
    | addMut == addMut' -> (Just [], []) -- cancels fwd
    | otherwise -> (Just [(addMut', ci <| nexts)], rsubs)
  (Just (addMut, Just (prvs, lsubs)), Just (addMut', nexts, rsubs))
    | not (null lsubs) -> (Nothing, lsubs ++ rsubs)
    | addMut == addMut' -> ( Just [(addMut, prvs <> (ci <| nexts))] -- join
                           , rsubs )
    | otherwise -> ( Just [ (addMut, prvs <> (ci:|[]))
                          , (addMut', ci <| nexts) ], rsubs )
  where
    prevCIs = prevMutCIsSub sub dly tst ci
    nextCIs = nextMutCIs sub dly tst ci

-- | Return each etc-add-in-add-in-etc-chain the given in-CI is the
-- canonical (read: first) in-CI of. (TODO: optimize case order)
composeAdds :: PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m [(Mutation, NonEmpty CI)]
composeAdds dly tst ci = (<$> liftA2 (,) prevCIs nextCIs) $ \case
  (Nothing, Nothing) -> []
  (Nothing, Just (addMut, nexts, _)) -> [(addMut, ci <| nexts)]
  (Just (_, Nothing), Nothing) -> []
  (Just (addMut, Just prv), Nothing) -> [(addMut, prv:|[ci])]
  (Just (addMut, Nothing), Just (addMut', nexts, _))
    | addMut == addMut' -> [] -- cancels fwd
    | otherwise -> [ (addMut', ci <| nexts) ]
  (Just (addMut, Just prv), Just (addMut', nexts, _))
    | addMut == addMut' -> [(addMut, prv <| ci <| nexts)] -- join
    | otherwise -> [ (addMut, prv:|[ci])
                   , (addMut', ci <| nexts) ]
  where
    prevCIs = prevMutCI dly tst ci
    nextCIs = nextMutCIs (\_ _ -> False) dly tst ci

-- WHERE --

-- | Return the etc-in-out-in-out chain (and the add-mutation that would
-- switch all returned out-intervals' membership) immediately preceding
-- the given in-interval, in left-to-right order (therefore always
-- ending in an out-interval immediately preceeding the given interval),
-- only if no in-interval has a joint failing the given `sub` condition.
--
-- Returns `Nothing` if there is no joint immediately preceeding the
-- given interval or if there is not add-mutation that would switch
-- it. Within the pair, returns Nothing if the chain is cancelled by
-- failing the `sub` condition, otherwise, returns the chain as a
-- NonEmpty and the interspersed set of sub-intervals as a (possibly
-- empty) list.
--
-- The `sub` condition is assumed to hold only joints which are members
-- of the state's type (hence "sub"): it's only checked once a joint has
-- been found to be within the state's type.
prevMutCIsSub :: forall m. PrimMonad m => (Sym -> Sym -> Bool) ->
  Doubly (PrimState m) -> TypeState (PrimState m) ->
  CI -> m (Maybe (Mutation, Maybe (NonEmpty CI, [CI])))
prevMutCIsSub sub str tst (CI hd0 shd0 _ _ _) = (D.prev str hd0 >>=) $ \case
  Nothing -> return Nothing -- no prev symbol/interval
  Just (phd0,sphd0) -> (TS.addMutOf tst sphd0 shd0 >>=) $ \case
    Nothing -> return Nothing -- no mut
    Just addMut -> let mkCI hd shd len = CI hd shd len hd0 shd0
                   in Just . (addMut,) <$> goOut [] [] mkCI phd0 sphd0 2
      where
        goOut :: [CI] -> [CI] -> (Index -> Sym -> Len -> CI) ->
                 Index -> Sym -> Len -> m (Maybe (NonEmpty CI, [CI]))
        goOut acc subs mkCI = go where
          go hd shd !len = (D.prev str hd >>=) $ \case
            Nothing -> return res -- hit start, end
            Just (phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
              True | sub sphd shd ->
                       let mkCI' hd' shd' len' = CI hd' shd' len' hd shd
                       in goSub (ci:acc) subs mkCI' phd sphd 2 -- sub: switch
                   | otherwise -> return Nothing -- not first of a chain, cancel
              False -> (TS.addMutOf tst sphd shd >>=) $ \case
                Just addMut' | addMut' == addMut -> go phd sphd (len+1)
                _else -> return res -- end of interval
            where
              ci = mkCI hd shd len
              res = Just (ci:|acc, subs)

        goSub :: [CI] -> [CI] -> (Index -> Sym -> Len -> CI) ->
                 Index -> Sym -> Len -> m (Maybe (NonEmpty CI, [CI]))
        goSub acc subs mkCI = go where
          go hd shd !len = (D.prev str hd >>=) $ \case
            Nothing -> return res -- hit start, end
            Just (phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
              True | sub sphd shd -> go phd sphd (len+1)
                   | otherwise -> return Nothing -- not first of a chain, cancel
              False -> (TS.addMutOf tst sphd shd >>=) $ \case
                Just addMut'
                  | addMut' == addMut ->
                      let mkCI' hd' shd' len' = CI hd' shd' len' hd shd
                      in goOut (ci:acc) (ci:subs) mkCI' phd sphd 2 -- switch
                _else -> return res -- end
            where
              ci = mkCI hd shd len
              res = Just (ci:|acc, ci:subs)

-- | Return the CI immediately preceeding the given CI and the
-- Add-mutation that would include it in the type, if there is such a
-- mutation. Returns Nothing inside the pair if there is another in-CI
-- immediately before that mutCI (signals cancel, for injectivity).
prevMutCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
              TypeState (PrimState m) -> CI -> m (Maybe (Mutation, Maybe CI))
prevMutCI str tst (CI hd0 shd0 _ _ _) = (D.prev str hd0 >>=) $ \case
  Nothing -> return Nothing -- no prev symbol/interval
  Just (phd0,sphd0) -> (TS.addMutOf tst sphd0 shd0 >>=) $ \case
    Nothing -> return Nothing -- no mut
    Just addMut -> Just . (addMut,) <$> go phd0 sphd0 2
      where
        go hd shd !len = (D.prev str hd >>=) $ \case
          Nothing -> return $ Just ci -- hit start, end
          Just (phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
            True -> return Nothing -- not first of a chain, cancel
            False -> (TS.addMutOf tst sphd shd >>=) $ \case
              Just addMut' | addMut' == addMut -> go phd sphd (len+1)
              _else -> return $ Just ci -- end of interval
          where
            ci = CI hd shd len hd0 shd0

-- | Given the string, joint type and an in-interval, return the longest
-- immediately following sequence of alternating
-- out-in-out-etc. intervals where all the out-intervals would get their
-- membership flipped (i.e. included) by the same add-mutation, which is
-- also returned. Return Nothing if end of string or if the following
-- joint does not have an add-mutation. Returns intervals satisfying the
-- `sub` predicate if they don't make contact with an in-CI (i.e. if
-- they wouldn't be part of any superCI)
nextMutCIs :: forall m. PrimMonad m => (Sym -> Sym -> Bool) ->
  Doubly (PrimState m) -> TypeState (PrimState m) -> CI ->
  m (Maybe (Mutation, NonEmpty CI, [CI]))
nextMutCIs sub str tst (CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (TS.addMutOf tst s0 s1 >>=) $ \case
    Nothing -> return Nothing -- no add-mutation
    Just addMut -> Just . uc (addMut,,) <$> goOut [] [] (CI i0 s0) 2 i1 s1
      where
        goOut :: [CI] -> [CI] -> (Len -> Index -> Sym -> CI) ->
                 Len -> Index -> Sym -> m (NonEmpty CI, [CI])
        goOut acc subs mkCI = go where
          go !len tl stl = (D.next str tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
              True | sub stl sntl -> -- switch
                       goSub False (ci:acc) subs mkCI' mkCI' 2 2 ntl sntl
                   | otherwise -> goIn (ci:acc) subs mkCI' 2 ntl sntl -- switch
              False -> (TS.addMutOf tst stl sntl >>=) $ \case
                Just addMut' | addMut' == addMut ->
                                 go (len+1) ntl sntl -- keep going
                _else -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = (NE.reverse (ci:|acc), reverse subs)

        -- in && not sub --
        goIn :: [CI] -> [CI] -> (Len -> Index -> Sym -> CI) ->
                Len -> Index -> Sym -> m (NonEmpty CI, [CI])
        goIn acc subs mkCI = go where
          go !len tl stl = (D.next str tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
              True | sub stl sntl -> goSub True acc subs mkCI' mkCI 2 2 ntl sntl
                   | otherwise -> go (len+1) ntl sntl -- keep going
              False -> (TS.addMutOf tst stl sntl >>=) $ \case
                Just addMut' | addMut' == addMut ->
                                 goOut (ci:acc) subs mkCI' 2 ntl sntl -- switch
                _else -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = (NE.reverse (ci:|acc), reverse subs)

        -- in && sub -- both inter (fromIn == False) and sub super (" == True)
        goSub :: Bool -> [CI] -> [CI] -> (Len -> Index -> Sym -> CI) ->
                 (Len -> Index -> Sym -> CI) -> Len -> Len ->
                 Index -> Sym -> m (NonEmpty CI, [CI])
        goSub fromIn acc subs mkSub mkCI = go where
          go !subLen !len tl stl = (D.next str tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
              True | sub stl sntl ->
                       go (subLen+1) (len+1) ntl sntl -- keep going
                   | otherwise -> -- sub is not inter-, don't cons
                       goIn acc subs mkCI (len+1) ntl sntl
              False -> (TS.addMutOf tst stl sntl >>=) $ \case
                Just addMut' | addMut' == addMut ->
                  goOut (ci:acc) subs' (CI tl stl) 2 ntl sntl -- switch
                _else -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              subs' | fromIn = subs
                    | otherwise = mkSub subLen tl stl : subs
              res = (NE.reverse (ci:|acc), reverse subs')

----------------------
-- ON DEL MUTATIONS --
----------------------

-- | Given a constructive interval of the joint type (in), count all the
-- differences in symbol counts between the symCounts of the CIs for all
-- joints removed by the same del-mutation and and the real difference
-- in symCounts from applying those mutations. Correction values are
-- signed in order to be added to the del-mut's CIs' counts *before*
-- they are added/subtracted from the string's or type's counts.
onDelMuts :: PrimMonad m =>
  Doubly (PrimState m) -> TypeState (PrimState m) -> CI -> m Cor
onDelMuts _ _ (CI _ _ 2 _ _) = return M.empty
onDelMuts dly tst supCI@(CI _ _ supLen supTl supStl) = do
  traceM ""
  traceM $ "[DEL COR]: " ++ show supCI
  res <- M.filter (not . IM.null) -- clean
         . fmap (IM.filter (/= 0) .  go)
         . M.fromListWith (<>)
         . reverse -- preserve order through (<>)
         . ffmap NE.singleton <$> decomposeIn dly tst supCI
  traceShowM res
  return res
  where
    supLenEven = even supLen
    go :: NonEmpty (Bool, CI) -> IntMap Int
    go = flip execState IM.empty . go_ False
      where -- False == aligned with supCI
        go_ :: Bool -> NonEmpty (Bool, CI) -> State (IntMap Int) ()
        go_ prevRemPhase ((hp, CI _ shd len tl stl) :| rest) = do
          let outOfPhase = prevRemPhase /= hp
          -- out of phase with rem means (phd,hd) (which is in phase)
          -- will still be constr after del mut; means hd will still be
          -- constr. so shd's count will not get docked by the mut
          when outOfPhase $ dec shd -- hd
          case rest of
            next:rest' ->
              when lenEven (dec stl)
              >> go_ nextRemPhase (next:|rest')

            _ | tl == supTl ->
                  let d = fromEnum supLenEven - fromEnum lenEven
                  in when (d /= 0) $ inc_ d stl

              | otherwise -> do -- a rem follows
                  when lenEven $ dec stl
                  let supTlSwitchedPhase = nextRemPhase
                  when supTlSwitchedPhase $
                    if supLenEven then inc supStl -- constr. -> non
                    else dec supStl -- non-constr. -> constr.
          where
            lenEven = even len -- means tl is constr.
            nextRemPhase =  -- | even len   = not hp
              lenEven /= hp -- | othwerwise = hp

    -- decrement: every symbol that is counted in the del CI, but will
    -- still be constr. in the remainder CI
    dec :: Sym -> State (IntMap Int) ()
    dec = inc_ (-1)
    -- increment: every symbol that is not counted in the del CI,
    -- but still gets its count reduced in the remainder CI
    inc :: Sym -> State (IntMap Int) ()
    inc = inc_ 1
    inc_ :: Int -> Sym -> State (IntMap Int) ()
    inc_ d s = modify $ IM.insertWith (+) s d

-- WHERE --

-- | Break an in-CI into an ordered (by tail, or by how early they end)
-- list of its segments by del-mutation. Only made for
-- @delMutCorrsOf@. For each CI also returns phase (binary) of the head
-- w.r.t. the begining of the given CI, i.e. 0\/False is even\/constr.,
-- 1\/True is odd\/non-constr.
decomposeIn :: PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m [(Mutation, (Bool, CI))]
decomposeIn str tst ci@(CI hd shd len _ stl)
  | len == 2  = (,(False,ci)) <<$>> TS.delMutsOf tst shd stl
  | otherwise = go [] False hd shd . drop 1 =<< CI.symExtension str ci
  where
    go mcis _ _ _ [] = return mcis
    go mcis p i0 s0 ((i1,s1):rest) = do
      muts <- TS.delMutsOf tst s0 s1
      let (alive, ended) = L.partition (flip elem muts . fst) mcis
          started = (, (p, CI i0 s0 2 i1 s1))
                    <$> filter (`notElem` (fst <$> mcis)) muts
          mcis' = (++ started) $ (<<<$>>> alive) $ \c ->
            c{ _ciLength = _ciLength c + 1 -- extend
             , _tailIndex = i1
             , _tailSymbol = s1 }
      (ended ++) <$> go mcis' (not p) i1 s1 rest

err :: [Char] -> a
err = error . ("Correction." ++)
