{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE LambdaCase, TupleSections, BangPatterns #-}

module Diagram.Evolution.Correction (module Diagram.Evolution.Correction) where

import Prelude hiding (init)

import Control.Monad
import Control.Monad.State.Strict

-- import Data.Maybe
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
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
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

-- onAllMuts :: PrimMonad m => ((Sym, Sym) -> Bool) -> Doubly (PrimState m) ->
--   TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
-- onAllMuts sub dly tst ci = do
--   onAdd <- onAddMuts sub dly tst ci
--   onDel <- onDelMuts dly tst ci
--   return $ M.unionWith (error "impossible") onAdd onDel

----------------------
-- ON ADD MUTATIONS --
----------------------

-- -- | Corrections on add-muts regarding all chains of a given CI. See
-- -- @onAddMuts_@.
-- onAddMuts :: PrimMonad m => ((Sym, Sym) -> Bool) -> Doubly (PrimState m) ->
--   TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
-- onAddMuts = fmap (unions . fmap (uc onAddMuts_)) .:: composeAdds
--   where unions = fromMaybe M.empty . foldTree union
--         union = M.unionWith (IM.unionWith (+))

-- WHERE --

-- | Given a non-empty list of overlapping/connecting intervals brought
-- together by an Add-mutation, alternating [in-]add-in-add-etc., return
-- the appropriate correction on the mut's CIs' sym counts as a
-- singleton of the given mutation, which is assumed to be the
-- Add-mutation bringing together the sequence of intervals, although
-- this is not checked. Correction values are signed in order to be
-- added to the mut's CIs' counts *before* they are added/subtracted
-- from the type's or string's counts.
onAddMuts_ :: Mutation -> NonEmpty CI -> Map Mutation (IntMap Int)
onAddMuts_ mut cis = M.singleton mut $ flip execState IM.empty $ do
  forM_ (NE.init cis) $ \(CI _ _ len _ stl) ->
    when (even len) $ modify $ IM.insertWith (+) stl (-1)
  let CI _ _ oldLen _ tailSym = NE.last cis
      newLen = sum (_ciLength <$> cis) -- constituents lengths
               - (length cis - 1) -- overlaps
      d = fromEnum (even newLen) - fromEnum (even oldLen)
  when (d /= 0) $ modify $ IM.insertWith (+) tailSym d

-- | Grab the largest chain possible, if CI is first in the chain (for
-- injectivity), for zero, one or two mutations (prec. & next, if they
-- are different), also returning the set of in-CIs satisfying the `sub`
-- predicate.
--
-- In some way this function is the inverse of @decomposeIn@. This chain
-- is the level at which add-mut corrections have to be calculated
-- because of how parity might cascade down an arbitrary number of type
-- CIs interspersed by mut-added CIs.
composeAdds :: PrimMonad m => ((Sym, Sym) -> Bool) ->
  Doubly (PrimState m) -> TypeState (PrimState m) ->
  CI -> m ([(Mutation, NonEmpty CI)], [CI])
composeAdds sub dly tst ci = (prevCIs >>=) $ \case
  Nothing -> return ([],[]) -- skip

  Just Nothing -> (<$> nextCIs) $ \case
    Nothing -> ([],[])
    Just (addMut, nexts, rsubs) -> ([(addMut, ci <| nexts)], rsubs)

  Just (Just (addMut, prv, lsubs)) -> (<$> nextCIs) $ \case
    Nothing -> ([(addMut, prv <> (ci:|[]))], lsubs)
    Just (addMut', nexts, rsubs)
      | addMut == addMut' -> ([(addMut, prv <> (ci <| nexts))], subs)
      | otherwise -> ( [ (addMut, prv <> (ci:|[]))
                       , (addMut', ci <| nexts) ], subs )
      where subs = lsubs ++ rsubs
  where
    prevCIs = prevMutCI  sub dly tst ci
    nextCIs = nextMutCIs sub dly tst ci

-- WHERE --

-- | Return the etc-in-out-in-out chain (and the add-mutation that would
-- switch all returned out-intervals' memberships) immediately preceding
-- the given in-interval, from left to right (therefore always ending in
-- an out-interval), only if no out interval fails the given `sub`
-- condition.
--
-- Returns `Nothing` if the chain was cancelled by failing the `sub`
-- condition, `Just Nothing` if there is no addable preceeding joint,
-- and `Just . Just` if there is such a mutation-chain pair.
--
-- The `sub` condition is assumed to hold only joints which are members
-- of the state's type (hence "sub"): it's only checked once a joint has
-- been found to be within the state's type.
prevMutCI :: forall m. PrimMonad m => ((Sym, Sym) -> Bool) ->
  Doubly (PrimState m) -> TypeState (PrimState m) ->
  CI -> m (Maybe (Maybe (Mutation, NonEmpty CI, [CI])))
prevMutCI sub str tst (CI hd0 shd0 _ _ _) = (D.prev str hd0 >>=) $ \case
  Nothing -> return $ Just Nothing -- no prev symbol/interval
  Just (phd0,sphd0) -> (TS.addMutOf tst sphd0 shd0 >>=) $ \case
    Nothing -> return $ Just Nothing -- no mut
    Just addMut -> let mkCI hd shd len = CI hd shd len hd0 shd0
                   in uc (addMut,,) <<<$>>> goOut [] [] mkCI phd0 sphd0 2
      where
        goOut :: [CI] -> [CI] -> (Index -> Sym -> Len -> CI) ->
                 Index -> Sym -> Len -> m (Maybe (Maybe (NonEmpty CI, [CI])))
        goOut acc subs mkCI = go where
          go hd shd !len = (D.prev str hd >>=) $ \case
            Nothing -> return res -- hit start, end
            Just p@(phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
              True | sub p ->
                       let mkCI' hd' shd' len' = CI hd' shd' len' hd shd
                       in goSub (ci:acc) subs mkCI' phd sphd 2 -- sub: switch
                   | otherwise ->
                       return Nothing -- not first of a chain (cancel)
              False -> (TS.addMutOf tst sphd shd >>=) $ \case
                Just addMut' | addMut' == addMut -> go phd sphd (len+1)
                _else -> return res -- end of interval
            where
              ci = mkCI hd shd len
              res = Just $ Just (ci:|acc, subs)

        goSub :: [CI] -> [CI] -> (Index -> Sym -> Len -> CI) ->
                 Index -> Sym -> Len -> m (Maybe (Maybe (NonEmpty CI, [CI])))
        goSub acc subs mkCI = go where
          go hd shd !len = (D.prev str hd >>=) $ \case
            Nothing -> return res -- hit start, end
            Just p@(phd,sphd) -> (TS.member tst sphd shd >>=) $ \case
              True | sub p -> go phd sphd (len+1)
                   | otherwise -> return Nothing
              False -> (TS.addMutOf tst sphd shd >>=) $ \case
                Just addMut'
                  | addMut' == addMut ->
                      let mkCI' hd' shd' len' = CI hd' shd' len' hd shd
                      in goOut (ci:acc) (ci:subs) mkCI' phd sphd 2 -- switch
                _else -> return res -- end
            where
              ci = mkCI hd shd len
              res = Just $ Just (ci:|acc, ci:subs)

-- | Given the string, joint type and an in-interval, return the longest
-- immediately following sequence of alternating
-- out-in-out-etc. intervals where all the out-intervals would get
-- their membership flipped (i.e. included) by the same add-mutation,
-- which is also returned. Return Nothing if end of string or if the
-- following joint does not have an add-mutation.
nextMutCIs :: forall m. PrimMonad m => ((Sym, Sym) -> Bool) ->
  Doubly (PrimState m) -> TypeState (PrimState m) -> CI ->
  m (Maybe (Mutation, NonEmpty CI, [CI]))
nextMutCIs sub str tst (CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (TS.addMutOf tst s0 s1 >>=) $ \case
    Nothing -> return Nothing -- no add-mutation
    Just addMut -> Just . uc (addMut,,) <$> goOut [] [] (CI s0 i0) 2 i1 s1
      where
        goOut :: [CI] -> [CI] -> (Len -> Index -> Sym -> CI) ->
                 Len -> Index -> Sym -> m (NonEmpty CI, [CI])
        goOut acc subs mkCI = go where
          go !len tl stl = (D.next str tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
              True | sub (stl,sntl) ->
                       goSub (ci:acc) subs mkCI' mkCI' 2 2 ntl sntl -- switch
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
              True | sub (stl,sntl) -> goSub acc subs mkCI' mkCI 2 2 ntl sntl
                   | otherwise -> go (len+1) ntl sntl -- keep going
              False -> (TS.addMutOf tst stl sntl >>=) $ \case
                Just addMut' | addMut' == addMut ->
                                 goOut (ci:acc) subs mkCI' 2 ntl sntl -- switch
                _else -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = (NE.reverse (ci:|acc), reverse subs)

        -- in && sub --
        goSub :: [CI] -> [CI] -> (Len -> Index -> Sym -> CI) ->
                 (Len -> Index -> Sym -> CI) -> Len -> Len ->
                 Index -> Sym -> m (NonEmpty CI, [CI])
        goSub acc subs mkSub mkCI = go where
          go !subLen !len tl stl = (D.next str tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (TS.member tst stl sntl >>=) $ \case
              True | sub (stl,sntl) ->
                       go (subLen+1) (len+1) ntl sntl -- keep going
                   | otherwise -> goIn acc subs' mkCI (len+1) ntl sntl
              False -> (TS.addMutOf tst stl sntl >>=) $ \case
                Just addMut' | addMut' == addMut ->
                  goOut (ci:acc) subs' (CI tl stl) 2 ntl sntl -- switch
                _else -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              subs' = mkSub subLen tl stl : subs
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
onDelMuts :: PrimMonad m => Doubly (PrimState m) ->
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

-- | Break an in-CI into an ordered (by tail, or by how early they end)
-- list of its segments by del-mutation. Only made for
-- @delMutCorrsOf@. For each CI also returns phase (binary) of the head
-- w.r.t. the begining of the given CI, i.e. 0\/False is even\/constr.,
-- 1\/True is odd\/non-constr.
decomposeIn :: PrimMonad m => Doubly (PrimState m) ->
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

----------
-- MORE --
----------

-- | For a string, a type state, a joint type **which is a subtype of
-- the type state**, and a continuous, a maximal constructive interval
-- (CI) in the subtype on the string, return the super-CI of the given
-- CI in the type state, but only if this super-CI doesn't contain
-- another CI member of the given joint (sub-)type on the left of the
-- given CI (for injectivity).
--
-- Explicitly: returns `Nothing` if the superCI is canonically mapped on
-- by another CI on its left, `Just Nothing` if the superCI is itself
-- (not strictly super), and `Just (Just _)` otherwise.
--
-- Returns the superCI (fst) and the remainder CIs from taking aways all
-- of the given JointType's intervals from the superCI (snd), in
-- left-to-right order.
superCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> JointType -> CI -> m (Maybe (Maybe (CI, [CI])))
superCI dly tst jt (CI hd0 shd0 len0 tl0 stl0) = do

  bwd <- (D.prev dly hd0 >>=) $ \case
    Nothing -> return $ Just Nothing -- same
    Just (phd, sphd) -> (TS.member tst sphd shd0 >>=) $ \case
      False -> return $ Just Nothing -- same
      True -> goBwd hd0 shd0 phd sphd 2 -- tl first

  case bwd of
    Nothing -> return Nothing -- canceled (escaladed from expandBwd)
    Just bwd' -> do
      fwd <- (D.next dly tl0 >>=) $ \case
        Nothing -> return Nothing -- same
        Just (ntl, sntl) -> (TS.member tst stl0 sntl >>=) $ \case
          False -> return Nothing -- same
          True -> Just <$> goFwd tl0 stl0 2 ntl sntl -- GT

      return $ Just $ case (bwd', fwd) of
        (Nothing, Nothing) -> Nothing -- same: Just Nothing
        (Nothing, Just (CI _ _ lenFwd tl stl, rems)) ->
          let len = len0 + lenFwd - 1
          in Just (CI hd0 shd0 len tl stl, rems)
        (Just lrem@(CI hd shd lenBwd _ _), Nothing) ->
          let len = lenBwd + len0 - 1
          in Just (CI hd shd len tl0 stl0, [lrem])
        (Just lrem@(CI hd shd lenBwd _ _), Just (CI _ _ lenFwd tl stl, rems)) ->
          let len = lenBwd + len0 + lenFwd - 2
          in Just (CI hd shd len tl stl, lrem:rems)
  where
    goBwd tl stl = go
      where
        go hd shd !len = (D.prev dly hd >>=) $ \case
          Nothing -> return $ Just $ Just ci -- eos
          Just (phd, sphd) -> (TS.member tst sphd shd >>=) $ \case
            False -> return $ Just $ Just ci -- end
            True | JT.member (sphd,shd) jt -> return Nothing -- canceled
                 | otherwise -> go phd sphd (len+1) -- continue
          where ci = CI hd shd len tl stl

    goFwd hd shd = goRem [] 1 hd shd -- (len+remLen-1) overlap logic
      where                          -- requires we start with len 1
        goRem rems len remHd remShd = go
          where -- a remainder is inside TypeState but outside jt
            go !remLen tl stl = (D.next dly tl >>=) $ \case
              Nothing -> return (super, reverse rems') -- eos
              Just (ntl, sntl) -> (TS.member tst stl sntl >>=) $ \case
                True | JT.member (stl,sntl) jt ->
                         goJT rems' (len+remLen-1) ntl sntl -- switch
                     | otherwise -> go (remLen+1) ntl sntl -- cont.
                False -> return (super, reverse rems') -- end
              where super = CI hd shd (len+remLen-1) tl stl
                    rems' = (CI remHd remShd remLen tl stl):rems

        goJT rems !len tl stl = (D.next dly tl >>=) $ \case
          Nothing -> return (super, reverse rems) -- eos
          Just (ntl, sntl) -> (TS.member tst stl sntl >>=) $ \case
            True | JT.member (stl,sntl) jt ->
                     goJT rems (len+1) ntl sntl -- cont.
                 | otherwise ->
                     goRem rems len tl stl 2 ntl sntl -- switch
            False -> return (super, reverse rems) -- end
          where super = CI hd shd len tl stl
