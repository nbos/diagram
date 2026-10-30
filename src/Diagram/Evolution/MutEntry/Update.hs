{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
{-# LANGUAGE LambdaCase, BangPatterns, TupleSections #-}
module Diagram.Evolution.MutEntry.Update (
  module Diagram.Evolution.MutEntry.Update ) where

import Debug.Trace
import Control.Lens hiding (Index, (<|))
import Control.Monad.Extra

import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NE

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive

import qualified Diagram.Doubly as D
import qualified Diagram.UnionType as UT
import qualified Diagram.JointType as JT
import Diagram.ConstrInterval (CI(..))
import qualified Diagram.ConstrInterval as CI
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math (logFact)
import Diagram.Evolution.Mutation (Mutation(..),MutType(..))
import qualified Diagram.Evolution.Mutation as Mut
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution.MutEntry (MutEntry(..))
import qualified Diagram.Evolution.Correction as Cor

import Diagram.Util

data Update = MEU

  { _onSymCounts :: !(IntMap (Count, Count, Double)) -- Interval and
    -- loss precomputed because there is a lot of sharing.

  , _onCorrection :: !(IntMap Int) -- Includes changes on correction
    -- caused by the introduction or removal of joints into the
    -- JointType through the application of a mutation (not the one
    -- being updated). Does not include changes on correction caused by
    -- the introduction or removal of joints into the CIs of the
    -- MutEntry.

  , _onCIs :: !CIs } -- CIs to be added or removed. Always only added
    -- following an Add mutation. Always only removed following a Del
    -- mutation.

  deriving (Show,Eq)
makeLenses ''Update

------------------
-- CONSTRUCTION --
------------------

empty :: Update
empty = MEU IM.empty IM.empty CIs.empty

fromDCounts :: IntMap (Count, Count, Double) -> Update
fromDCounts dnIls = empty{ _onSymCounts = dnIls }

fromDCor :: IntMap Int -> Update
fromDCor ddnIls = empty{ _onCorrection = ddnIls }

fromDCIs :: CIs -> Update
fromDCIs cis = empty{ _onCIs = cis }

-----------
-- APPLY --
-----------
--  +--------------[ mut :: {Add|Del} ]--------------+
--  |                                                |
--  |      ddns :: ns' -> ns''   dnm :: nm -> nm'    |
--  |                                                |
--  |      / .[forall sym]. \                        |
--  |      | |  +--- n' ! | |       /  +--- nm ! \   |
--  |  log | | ddn  ----- | | + log | dnm  ----- |   |
--  |      | |  +--> n''! | |       \  +--> nm'! /   |
--  |      \ +------------+ /  |                  |  |
--  |         n_i for i < m    +-- new term n_m --+  |
--  |  where:                                        |
--  |    ddn = sign[Add/Del] * (CIs.symCounts + cor) |
--  |    dnm = (-1) * sum ddns `div` 2               |
--  |                                                |
--  +-----[ mutLoss = dnsLoss + dnmLoss ]------------+

-- | Given a membership function of the type prior to the application of
-- the mutation at the source of this update, a function for the
-- post-intro count of symbols unaffected by the applied mutation (so
-- whether it's from the TypeState before or after the application
-- doesn't matter), and a reference string, apply the given
-- MutEntryUpdate to the given MutEntry.
apply :: forall m. PrimMonad m => TypeState (PrimState m) -> MutType ->
  TypeState (PrimState m) -> (Sym -> Count) -> Doubly (PrimState m) ->
  MutEntry -> Update -> m MutEntry
apply oldTS prevMutType newTS n'Of dly me meu = do
  let ME mut oldDnsLoss oldDdns oldDnm oldCIs = me
      MEU n'Ils dCor dCIs@(CIs dCIsJT dCIs_ns dCIs_bhd _) = meu

  str <- D.toList dly

  traceM $ "\nApplying mut entry update (on " ++ pShow mut ++ "): " ++ pShow meu
  traceM $ "\noldCIs = " ++ pShow oldCIs
  traceM $ pShowStr (oldCIs^.CIs.jointType) str

  traceM $ "\ndCIs = " ++ pShow dCIs
  traceM $ pShowStr (dCIs^.CIs.jointType) str

  -- cis --
  (newCIs@(CIs new_mutJT _ _ _), _) <- case prevMutType of
    Add -> return $ CIs.join_ oldCIs dCIs
    Del -> (<$> CIs.difference dly Nothing Nothing oldCIs dCIs) $
      (_1.CIs.jointType %~ case mut of -- manually fix the joint type of CIs
        AddLeft _  -> JT.right %~ flip UT.differenceSubset (dCIsJT^.JT.right)
        DelLeft _  -> JT.right %~ flip UT.differenceSubset (dCIsJT^.JT.right)
        AddRight _ -> JT.left  %~ flip UT.differenceSubset (dCIsJT^.JT.left)
        DelRight _ -> JT.left  %~ flip UT.differenceSubset (dCIsJT^.JT.left)
        _else -> id -- no-op (dCIs should be empty: Add/Del2 only have 1 joint)
      )

  traceM $ "\nnew_cis = " ++ pShow newCIs
  traceM $ pShowStr (newCIs^.CIs.jointType) str

  -- dCor' --
  let
    smallerTS, largerTS :: TypeState (PrimState m)
    (smallerTS, largerTS) = case prevMutType of
      Add -> (oldTS, newTS)
      Del -> (newTS, oldTS)

    -- it's pointless to go CI-by-CI, rejecting them all if all joints
    -- of dCIs are joints of prevMut, so we check for this "overlap" as
    -- an optimization; should be removable with no change to result
    memPrevMutLeft, memPrevMutRight :: Sym -> m Bool
    memPrevMutLeft s0 = (TS.leftMember largerTS s0 >>=) $ \case
      False -> return False -- i.e. liftA2 (&&)
      True  -> not <$> TS.leftMember smallerTS s0
    memPrevMutRight s1 = (TS.rightMember largerTS s1 >>=) $ \case
      False -> return False -- i.e. liftA2 (&&)
      True  -> not <$> TS.rightMember smallerTS s1
    leftMutOverlapCheck, rightMutOverlapCheck :: Sym -> m Bool
    leftMutOverlapCheck s0 = (memPrevMutLeft s0 >>=) $ \case
      False -> return False -- i.e. liftA2 (&&)
      True  -> allM memPrevMutRight (UT.toList $ dCIsJT^.JT.right)
    rightMutOverlapCheck s1 = (memPrevMutRight s1 >>=) $ \case
      False -> return False -- i.e. liftA2 (&&)
      True  -> allM memPrevMutLeft (UT.toList $ dCIsJT^.JT.left)
  completeOverlap <- case mut of
    AddLeft  s0 ->  leftMutOverlapCheck s0
    DelLeft  s0 ->  leftMutOverlapCheck s0
    AddRight s1 -> rightMutOverlapCheck s1
    DelRight s1 -> rightMutOverlapCheck s1
    _else | dCIs == CIs.empty -> return True
          | otherwise -> err' $ "bi-directional mut only expected to "
                         ++ "have one joint: " ++ show (mut, dCIs)
    --
  let
    thisMutType = Mut.typeOfMut mut
    memA = return .: JT.member dCIsJT
    memB s0 s1 = let
      memNewTS = (<$> TS.member newTS s0 s1)
      memNewMutJT = JT.member new_mutJT s0 s1
      mem_dCIsJT = JT.member dCIsJT s0 s1
      in case (prevMutType, thisMutType) of
        (Add, Add) -> memNewTS (||      memNewMutJT)
        (Add, Del) -> memNewTS (&& not  memNewMutJT)
        (Del, Add) -> memNewTS (||     (memNewMutJT && not mem_dCIsJT)) -- old?
        (Del, Del) -> memNewTS (&& not (memNewMutJT ||     mem_dCIsJT))
  let
    dCIsL = IM.elems dCIs_bhd
    sign | prevMutType == thisMutType = id
         | otherwise = negate :: Int -> Int

    memPrevMut :: Sym -> Sym -> m Bool
    memPrevMut s0 s1 = (TS.member largerTS s0 s1 >>=) $ \case
      False -> return False
      True  -> not <$> TS.member smallerTS s0 s1

  traceM $ "\n dCIsL = " ++ pShow dCIsL

  traceM "\ncompose output:"
  dCor' <- case completeOverlap of
    True -> return IM.empty
    False -> ffmap sign $
      ( fmap ( fromMaybe IM.empty . foldTree imUnion -- join
               . fmap Cor.onAddMut_ ) -- get Cor
        . filterM (fmap not . overlapsPrevMut dly memPrevMut) -- no prev mut
        <=< fmap (catMaybes . catMaybes . fmap traceShowId) -- no `Nothing` or `Just Nothing`
        . mapM (compose dly memA memB) ) dCIsL

  traceM $ "\ndCor' = " ++ pShow dCor'

  let
    -- ddns --
    newDdns = IM.mergeWithKey (\_ (_, ddn') _ -> nothingIf (==0) ddn')
               (snd <$>) id ddnsIls oldDdns -- left-biased union
    ddnsIls = IM.mergeWithKey (\_ ddn d -> Just (ddn, ddn + d))
              (const IM.empty) ((0,) <$>) oldDdns dddns
    dddns = case thisMutType of
      Add -> negate <$> udddns
      Del -> udddns
    udddns = dCIsCounts `imUnion` dCor `imUnion` dCor'
    dCIsCounts = case prevMutType of
      Add -> dCIs_ns
      Del -> negate <$> dCIs_ns

    -- dnsLoss --
    newDnsLoss = oldDnsLoss + dDnsLoss
    dDnsLoss = sum $ IM.mergeWithKey
               ( \s ((old_n', new_n', dLoss), _) (ddn, ddn') -> Just $
                 let old_n'' = old_n' + ddn
                 -- old_loss = logFact old_n' - logFact old_n''
                     new_n'' = new_n' + ddn'
                 -- new_loss = logFact new_n' - logFact new_n''
                 in traceShow ("s, both",s) $
                    traceShow ("old_n'",old_n') $
                    traceShow ("new_n'",new_n') $
                    traceShow ("dLoss",dLoss) $
                    traceShow ("ddn",ddn) $
                    traceShow ("ddn'",ddn') $
                    traceShow ("old_n''",old_n'') $
                    traceShow ("new_n''",new_n'') $
                    trace ""
                    dLoss - logFact new_n'' + logFact old_n'' )
               ( IM.mapWithKey $ \s ((old_n', new_n', dLoss), ddn) -> -- n'Il only
                   let old_n'' = old_n' + ddn
                       new_n'' = new_n' + ddn
                   in traceShow ("s, n'IlDdns",s) $
                      traceShow ("old_n'",old_n') $
                      traceShow ("new_n'",new_n') $
                      traceShow ("dLoss",dLoss) $
                      traceShow ("ddn",ddn) $
                      traceShow ("old_n''",old_n'') $
                      traceShow ("new_n''",new_n'') $
                      trace ""
                      dLoss - logFact new_n'' + logFact old_n'' )
               ( IM.mapWithKey $ \s (ddn, ddn') -> -- ddns only
                   let n'      = n'Of s -- old_n' == new_n'
                       old_n'' = n' + ddn
                       new_n'' = n' + ddn'
                   in traceShow ("s, ddnsIls",s) $ -- ("s, ddnsIls",105)
                      traceShow ("ddn",ddn) $ -- ("ddn",0)
                      traceShow ("ddn'",ddn') $ -- ("ddn'",-2)
                      traceShow ("n'", n') $ -- ("n'",0)
                      traceShow ("old_n''",old_n'') $ -- ("old_n''",0)
                      traceShow ("new_n''",new_n'') $ -- ("new_n''",-2)
                      trace ""
                      logFact old_n'' - logFact new_n'' ) -- ERROR --

               n'IlDdns ddnsIls

    n'IlDdns = IM.mergeWithKey (const $ Just .: (,))
               (error "informed of a n' update that is not in ddns")
               (const $ IM.empty)
               n'Ils oldDdns

    -- dnm --
    newDnm = oldDnm + dDnm
    dDnm = negate (sum dddns) & \r ->
      if even r then r `div` 2
      else err' $ "expected even number: " ++ show (r, dddns)

  CIs.checkIntegrityM dly newCIs -- (debug)
  return $ ME mut newDnsLoss newDdns newDnm newCIs

  where
    err' = err . ("apply: " ++)

err :: String -> a
err = error . ("MutEntry.Update." ++)

overlapsPrevMut :: PrimMonad m => Doubly (PrimState m) ->
                   (Sym -> Sym -> m Bool) -> NonEmpty CI -> m Bool
overlapsPrevMut dly memPrevMut cis =
  overlapsPrevMut1 dly memPrevMut `anyM` NE.toList cis

overlapsPrevMut1 :: PrimMonad m => Doubly (PrimState m) ->
                    (Sym -> Sym -> m Bool) -> CI -> m Bool
overlapsPrevMut1 dly memPrevMut ci =
  CI.jointExtension dly ci >>= anyM (uc memPrevMut)

-- NOTE 1: this below could well be in Diagram.Evolution.Correction, but
-- as it is only used here (for now), having what is essentially
-- `composeAdds` on a single mutation (the one being updated) with a
-- particular filter operation in this file probably makes the reading
-- less confusing.

-- NOTE 2: we would use the super/sub separation as it is used in
-- Diagram.Evolution.Correction, i.e. that `sub ==> super`, or that the
-- CI chains are of alternating `sub` and `super & (not sub)`, but
-- because of the calculation of `dCor'` has to change the two
-- predicates depending on the 4 ways `prevMutType x thisMutType` can
-- go, would require a 3-part inclusion/exclusion definition of
-- predicates for `super`, it becomes simpler that the primary predicate
-- `super` not subsume the secondary predicate `sub`, so that the
-- returned CI chains are of alternating `sub` and `super` (instead of
-- `super & (not sub)`).
--
-- Predicate names, now being mutually exclusive sets, are renamed `inA`
-- for `sub` (`A` is the name given to the set where the given CI
-- belongs) and `inB` for `super`. This makes the logic easier to
-- follow.

-- | Given membership function for two mutually exclusive joint types, a
-- reference string, and a CI of joints satisfying the first membership
-- function, return the list of alternating [B-]A-B-A-B-etc. CIs,
-- surrounding (including) the given A-CI.
--
-- `Nothing` if another A-CI is to the left of the given CI separated by
-- a single B-CI, for injectivity. Or if any encountered joint
-- (including those of the given CI) satisfies `memPrevMut` predicate.
--
-- `Just Nothing` if the CI chain only contains the given CI (usual
-- case).
compose :: PrimMonad m => Doubly (PrimState m) -> (Sym -> Sym -> m Bool) ->
           (Sym -> Sym -> m Bool) -> CI -> m (Maybe (Maybe (NonEmpty CI)))
compose dly memA memB ciA = (<$> liftA2 (,) prev next) $ \case
    (Just Nothing, Nothing) -> Just Nothing -- no B-neighbors (usual case)
    (Just Nothing, Just nxt) -> Just $ Just (ciA <| nxt)
    (Just (Just prv), Nothing) -> Just $ Just (prv:|[ciA])
    (Just (Just prv), Just nxt) -> Just $ Just $ ((prv:|[ciA]) <> nxt)
    (Nothing, _) -> Nothing -- cancelled (rare)
  where
    prev = prevCI  memA memB dly ciA
    next = nextCIs memA memB dly ciA

-- WHERE --

prevCI :: PrimMonad m => (Sym -> Sym -> m Bool) -> (Sym -> Sym -> m Bool) ->
          Doubly (PrimState m) -> CI -> m (Maybe (Maybe CI))
prevCI memA memB dly (CI hd0 shd0 _ _ _) = (mmphd0 >>=) $ \case
  Nothing -> return $ Just Nothing -- no prev symbol/interval
  Just (phd0,sphd0) -> (memB sphd0 shd0 >>=) $ \case
    False -> return $ Just Nothing
    True -> go phd0 sphd0 2 -- Just <<$>> (go ... :: m (Maybe CI))
      where
        go hd shd !len = (D.prev dly hd >>=) $ \case
          Nothing -> return res -- hit start, end
          Just (phd,sphd) -> (memB sphd shd >>=) $ \case
            True -> go phd sphd (len+1)
            False -> (memA sphd shd >>=) $ \case
              True -> return Nothing -- not first of a chain, cancel
              False -> return res -- end of interval
          where
            res = Just $ Just $ CI hd shd len hd0 shd0
  where
    mmphd0 = D.prev dly hd0

nextCIs :: forall m. PrimMonad m => (Sym -> Sym -> m Bool) ->
           (Sym -> Sym -> m Bool) -> Doubly (PrimState m) -> CI ->
           m (Maybe (NonEmpty CI))
nextCIs memA memB dly (CI _ _ _ i0 s0) = (D.next dly i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (memB s0 s1 >>=) $ \case
    False -> return Nothing
    True -> Just <$> goB [] (CI i0 s0) 2 i1 s1
      where
        goB :: [CI] -> (Len -> Index -> Sym -> CI) ->
               Len -> Index -> Sym -> m (NonEmpty CI)
        goB acc mkCI = go where
          go !len tl stl = (D.next dly tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (memB stl sntl >>=) $ \case
              True -> go (len+1) ntl sntl -- keep going
              False -> (memA stl sntl >>=) $ \case
                True -> goA (ci:acc) mkCI' 2 ntl sntl -- switch
                False -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = NE.reverse (ci:|acc)

        goA :: [CI] -> (Len -> Index -> Sym -> CI) ->
               Len -> Index -> Sym -> m (NonEmpty CI)
        goA acc mkCI = go where
          go !len tl stl = (D.next dly tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (memA stl sntl >>=) $ \case
              True -> go (len+1) ntl sntl -- keep going
              False -> (memB stl sntl >>=) $ \case
                True -> goB (ci:acc) mkCI' 2 ntl sntl -- switch
                False -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = NE.reverse (ci:|acc)
