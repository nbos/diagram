{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, BangPatterns, TupleSections #-}
module Diagram.Evolution.MutEntry.Update (
  module Diagram.Evolution.MutEntry.Update ) where

import Debug.Trace

import Control.Lens hiding (Index, (<|))

import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty (NonEmpty(..),(<|))
import qualified Data.List.NonEmpty as NE

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive

import qualified Diagram.Doubly as D
import qualified Diagram.JointType as JT
import Diagram.ConstrInterval (CI(..))
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs
import Diagram.JointType (JointType)

import Diagram.Evolution.Math (logFact)
import Diagram.Evolution.Mutation (MutType(..))
import qualified Diagram.Evolution.Mutation as Mut
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
--  |      \ +------------+ /                        |
--  |                                                |
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
apply :: PrimMonad m => (JointType, Sym -> Sym -> m Bool) -> MutType ->
         (JointType, Sym -> Sym -> m Bool) -> (Sym -> Count) -> Doubly (PrimState m)
  -> MutEntry -> Update -> m MutEntry
apply (old_typJT, old_mem) prevMutType (new_typJT, new_mem) n'Of dly me meu = do
  let ME mut old_dnsLoss old_ddns old_dnm old_cis@(CIs old_mutJT _ _ _) = me
      MEU n'Ils dCor dCIs@(CIs dcis_jt dcis_ns dcis_bhd _) = meu

  -- cis --
  (new_cis@(CIs new_mutJT _ _ _), _some_cor) <- case prevMutType of
    -- we can get the cor terms from merging the delta with the mutJT,
    -- but it won't contain the ones from merging that with the typJT,
    -- so the cor on the introduced/eliminated joints has to be computed
    -- from scratch (dCor')
    Add -> return $ CIs.join_ old_cis dCIs
    Del -> CIs.difference dly (Just old_mem) Nothing old_cis dCIs

  -- (debug)
  str <- D.toList dly
  traceM "\nOld:"
  traceM $ pShowStrMut mut old_mutJT old_typJT str
  traceM "\nNew:"
  traceM $ pShowStrMut mut new_mutJT new_typJT str
  traceM $ pShow me
  traceM $ pShow meu
  --

  let super s0 s1 = (JT.member new_mutJT s0 s1 ||) <$> new_mem s0 s1
      sub = return .: JT.member dcis_jt
  dCor' <- fromMaybe IM.empty . foldTree imUnion
           . fmap Cor.onAddMut_ . catMaybes . catMaybes . traceShowId
           <$> mapM (compose super sub dly) (IM.elems dcis_bhd)
  traceM $ pShow dCor'

  let
    -- ddns --
    new_ddns = IM.mergeWithKey (\_ (_, ddn') _ -> nothingIf (==0) ddn')
               (snd <$>) id ddnsIls old_ddns -- left-biased union
    ddnsIls = IM.mergeWithKey (\_ ddn d -> Just (ddn, ddn + d))
              (const IM.empty) ((0,) <$>) old_ddns dddns
    dddns = case Mut.typeOfMut mut of
      Add -> negate <$> udddns
      Del -> udddns
    udddns = dCIsCounts `imUnion` dCor
    dCIsCounts = case prevMutType of
      Add -> dcis_ns'
      Del -> negate <$> dcis_ns'
    dcis_ns' = dcis_ns `imUnion` dCor'

    -- dnsLoss --
    new_dnsLoss = old_dnsLoss + dDnsLoss
    dDnsLoss = sum $ IM.mergeWithKey
               ( \_ (old_n', new_n', dLoss) (ddn, ddn') -> Just $
                 let old_n'' = old_n' + ddn
                 -- old_loss = logFact old_n' - logFact old_n''
                     new_n'' = new_n' + ddn'
                 -- new_loss = logFact new_n' - logFact new_n''
                 in dLoss - logFact new_n'' + logFact old_n'' )
               ( const IM.empty ) -- no n' change, no ddn ==> no dnsLoss
               ( IM.mapWithKey $ \s (ddn, ddn') -> -- ddns only
                   let n'      = n'Of s -- old == new
                       old_n'' = n' + ddn
                       new_n'' = n' + ddn'
                   in trace (pShow ("s",s)) $
                      trace (pShow ("ddn",ddn)) $
                      trace (pShow ("ddn'",ddn')) $
                      trace (pShow ("n'",n')) $
                      trace (pShow ("old_n''",old_n'')) $
                      trace (pShow ("new_n''",new_n'')) $
                      logFact old_n'' - logFact new_n'' )
               n'Ils ddnsIls

    -- dnm --
    new_dnm = old_dnm + dDnm
    dDnm = negate (sum dddns) & \r ->
      if even r then r `div` 2
      else err' $ "expected even number: " ++ show (r, dddns)

  return $ ME mut new_dnsLoss new_ddns new_dnm new_cis

  where
    err' = err . ("apply: " ++)

err :: String -> a
err = error . ("MutEntry.Update." ++)

-- | Returns the list of alternating [out-]in-out-etc. CIs, where each
-- in-CI, is in both `sub` and `super` types and out-CIs are in `super`,
-- but not `sub`, surrounding a given in-CI. Each joint member of `sub`
-- is assumed to be member of `super` as well. Returns Nothing if given
-- in-CI is member of a chain that will be returned by another (left)
-- in-CI (injectivity). Returns Just Nothing if the CI chain only
-- contains the given CI (singleton).
compose :: PrimMonad m => (Sym -> Sym -> m Bool) -> (Sym -> Sym -> m Bool) ->
           Doubly (PrimState m) -> CI -> m (Maybe (Maybe (NonEmpty CI)))
compose super sub dly ci = (<$> liftA2 (,) prev next) $ \case
  (Nothing, _) -> Nothing -- cancelled (rare)
  (Just Nothing, mnxt) -> Just $ (ci <|) <$> mnxt
  (Just (Just prv), mnxt) -> Just $ Just $
                             maybe (prv:|[ci]) ((prv:|[ci]) <>) mnxt
  where
    prev = prevCI  super sub dly ci
    next = nextCIs super sub dly ci


-- WHERE --

prevCI :: PrimMonad m => (Sym -> Sym -> m Bool) -> (Sym -> Sym -> m Bool) ->
          Doubly (PrimState m) -> CI -> m (Maybe (Maybe CI))
prevCI super sub dly (CI hd0 shd0 _ _ _) = (D.prev dly hd0 >>=) $ \case
  Nothing -> return $ Just Nothing -- no prev symbol/interval
  Just (phd0,sphd0) -> (super sphd0 shd0 >>=) $ \case
    False -> return $ Just Nothing
    True -> go phd0 sphd0 2 -- Just <<$>> (go ... :: m (Maybe CI))
      where
        go hd shd !len = (D.prev dly hd >>=) $ \case
          Nothing -> return res -- hit start, end
          Just (phd,sphd) -> (super sphd shd >>=) $ \case
            True -> (sub sphd shd >>=) $ \case
              True -> return Nothing -- not first of a chain, cancel
              False -> go phd sphd (len+1)
            False -> return res -- end of interval
          where
            res = Just $ Just $ CI hd shd len hd0 shd0

nextCIs :: forall m. PrimMonad m => (Sym -> Sym -> m Bool) ->
           (Sym -> Sym -> m Bool) -> Doubly (PrimState m) ->
           CI -> m (Maybe (NonEmpty CI))
nextCIs super sub dly (CI _ _ _ i0 s0) = (D.next dly i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (super s0 s1 >>=) $ \case
    False -> return Nothing
    True -> Just <$> goOut [] (CI i0 s0) 2 i1 s1
      where
        goOut :: [CI] -> (Len -> Index -> Sym -> CI) ->
                 Len -> Index -> Sym -> m (NonEmpty CI)
        goOut acc mkCI = go where
          go !len tl stl = (D.next dly tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (super stl sntl >>=) $ \case
              True -> (sub stl sntl >>=) $ \case
                True -> goIn (ci:acc) mkCI' 2 ntl sntl -- switch
                False -> go (len+1) ntl sntl -- keep going
              False -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = NE.reverse (ci:|acc)

        goIn :: [CI] -> (Len -> Index -> Sym -> CI) ->
                Len -> Index -> Sym -> m (NonEmpty CI)
        goIn acc mkCI = go where
          go !len tl stl = (D.next dly tl >>=) $ \case
            Nothing -> return res -- hit end of string
            Just (ntl,sntl) -> (super stl sntl >>=) $ \case
              True -> (sub stl sntl >>=) $ \case
                True -> go (len+1) ntl sntl -- keep going
                False -> goOut (ci:acc) mkCI' 2 ntl sntl -- switch
              False -> return res -- end of intervals
            where
              ci = mkCI len tl stl
              mkCI' = CI tl stl
              res = NE.reverse (ci:|acc)
