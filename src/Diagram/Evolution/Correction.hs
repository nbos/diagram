{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

module Diagram.Evolution.Correction (module Diagram.Evolution.Correction) where

import Prelude hiding (init)

import Control.Monad
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
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

onAllMuts :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
onAllMuts = undefined -- TODO: delete

----------------------
-- ON ADD MUTATIONS --
----------------------

-- | Given a non-empty list of overlapping/connecting intervals brought
-- together by an Add-mutation, alternating [in-]add-in-add-etc., return
-- the appropriate correction on the mut's CIs' sym counts as a
-- singleton of the given mutation, which is assumed to be the
-- Add-mutation bringing together the sequence of intervals, although
-- this is not checked. Correction values are signed in order to be
-- added to the mut's CIs' counts *before* they are added/subtracted
-- from the type's or string's counts.
onAddMuts :: Mutation -> NonEmpty CI -> Map Mutation (IntMap Int)
onAddMuts mut cis = M.singleton mut $ flip execState IM.empty $ do
  forM_ (NE.init cis) $ \(CI _ _ len _ stl) ->
    when (even len) $ modify $ IM.insertWith (+) stl (-1)
  let CI _ _ oldLen _ tailSym = NE.last cis
      newLen = sum (_ciLength <$> cis) -- constituents lengths
               - (length cis - 1) -- overlaps
      d = fromEnum (even newLen) - fromEnum (even oldLen)
  when (d /= 0) $ modify $ IM.insertWith (+) tailSym d

-- | Grab the largest chain possible, if CI is first in the chain (for
-- injectivity), for a maximum of two mutations (prec. & next, if they
-- are different). In some way this function is the inverse of
-- @decomposeIn@.
composeAdds :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m [(Mutation, NonEmpty CI)]
composeAdds dly tst ci = (join <$> prevMutCI dly tst ci >>=) $ \case
  Nothing -> (<$> nextCIs ci) $ \case
    Nothing -> []
    Just (addMut, nexts) -> [ (addMut, ci:|nexts) ]

  Just (addMut, prv) -> (<$> nextCIs ci) $ \case
    Nothing -> [ (addMut, prv:|[ci]) ]
    Just (addMut', nexts)
      | addMut == addMut' -> [ (addMut, prv:|ci:nexts) ]
      | otherwise -> [ (addMut, prv:|[ci])
                     , (addMut', ci:|nexts) ]
  where
    nextCIs = nextMutCIs dly tst

-- WHERE --

-- | Return the out-interval (and the add-mutation that would switch its
-- membership) immediately preceding the given in-interval but only if
-- the out-interval is not itself preceded by another in-interval (will
-- get caught by nextMutCIs instead). Returns `Nothing` if the preceding
-- interval is so sandwitched, `Just Nothing` if there is no addable
-- preceeding joint, and `Just . Just` if there is such a
-- mutation-interval pair.
prevMutCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Maybe (Maybe (Mutation, CI)))
prevMutCI str tst (CI tl stl _ _ _) = (D.prev str tl >>=) $ \case
  Nothing -> return $ Just Nothing -- no prev symbol/interval
  Just (ptl,sptl) -> (TS.addMutOf tst sptl stl >>=) $ \case
    Nothing -> return $ Just Nothing -- no mut
    Just mut -> (mut,) <<<$>>> go 2 ptl sptl
      where
        go !len hd shd = (D.prev str hd >>=) $ \case
          Nothing -> return $ Just $ Just ci -- hit start, end
          Just (phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
            True -> return Nothing -- not first of a chain (cancel)
            False -> (TS.addMutOf tst sphd shd >>=) $ \case
              Just mut' | mut' == mut -> go (len+1) phd sphd
              _else -> return $ Just $ Just ci -- end of interval
          where
            ci = CI hd shd len tl stl

-- | Given the string, joint type and an in-interval, return the longest
-- immediately following sequence of alternating out-, int-, out-,
-- etc. intervals where all the out-intervals would get their membership
-- flipped (i.e. included) by the same add-mutation, which is also
-- returned. Return Nothing if end of string or if the following joint
-- does not have an add-mutation.
nextMutCIs :: forall m. PrimMonad m => Doubly (PrimState m) ->
              TypeState (PrimState m) -> CI -> m (Maybe (Mutation, [CI]))
nextMutCIs str tst (CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (TS.addMutOf tst s0 s1 >>=) $ \case
    Nothing -> return Nothing -- no add-mutation
    Just addMut -> Just . (addMut,) <$> grabOut [] (CI s0 i0) 2 i1 s1
      where
        grabOut :: [CI] -> (Len -> Index -> Sym -> CI) ->
                   Len -> Index -> Sym -> m [CI]
        grabOut acc mkCI !len tl stl = (D.next str tl >>=) $ \case
          Nothing -> return $ reverse acc' -- hit end of string
          Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
            True -> grabIn acc' (CI tl stl) 2 ntl sntl -- switch
            False -> (TS.addMutOf tst stl sntl >>=) $ \case
              Just addMut' | addMut' == addMut ->
                grabOut acc mkCI (len+1) ntl sntl -- keep going
              _else -> return $ reverse acc' -- end of intervals
          where
            acc' = mkCI len tl stl : acc

        grabIn :: [CI] -> (Len -> Index -> Sym -> CI) ->
                  Len -> Index -> Sym -> m [CI]
        grabIn acc mkCI !len tl stl = (D.next str tl >>=) $ \case
          Nothing -> return $ reverse acc' -- hit end of string
          Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
            True -> grabIn acc mkCI (len+1) ntl sntl -- keep going
            False -> (TS.addMutOf tst stl sntl >>=) $ \case
              Just addMut' | addMut' == addMut ->
                grabOut acc' (CI tl stl) 2 ntl sntl -- switch
              _else -> return $ reverse acc' -- end of intervals
          where
            acc' = mkCI len tl stl : acc

----------------------
-- ON DEL MUTATIONS --
----------------------

-- | Given a constructive interval of the joint type (in), count all the
-- differences in symbol counts between the symCounts of the CIs for all
-- joints removed by the same del-mutation and and the real difference
-- in symCounts from applying those mutations. Correction values are
-- signed in order to be added to the del-mut's CIs' counts *before*
-- they are added/subtracted from the string's or type's counts.
onDelMuts :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
onDelMuts dly tst supCI@(CI _ _ supLen supTl supStl) = do
  fmap go . M.fromListWith (<>)
    . reverse -- preserve order through (<>)
    . ffmap NE.singleton <$> decomposeIn dly tst supCI
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

-- | Break an in-CI into an ordered (by tail, then head) list of its
-- segments by del-mutation. Only made for @delMutCorrsOf@. For each CI
-- also returns phase (binary) of the head w.r.t. the begining of the
-- given CI, i.e. 0\/False is even\/constr., 1\/True is odd\/non-constr.
decomposeIn :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m [(Mutation, (Bool, CI))]
decomposeIn str tst ci@(CI hd shd len tl _)
  | len == 2  = (,(False,ci)) <<$>> TS.delMutsOf tst hd tl
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
