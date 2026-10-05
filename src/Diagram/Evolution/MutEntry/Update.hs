{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Diagram.Evolution.MutEntry.Update (
  module Diagram.Evolution.MutEntry.Update ) where

import Control.Lens
import Debug.Trace

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Pretty
import Diagram.String
import Diagram.Primitive

import qualified Diagram.Doubly as D
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs
import Diagram.JointType (JointType)

import Diagram.Evolution.Math ( logFact )
import Diagram.Evolution.Mutation (MutType(..))
import qualified Diagram.Evolution.Mutation as Mut
import Diagram.Evolution.MutEntry (MutEntry(..))

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
apply :: PrimMonad m => JointType -> MutType -> JointType ->
  (Sym -> Sym -> m Bool) -> (Sym -> Count) ->
  Doubly (PrimState m) -> MutEntry -> Update -> m MutEntry
apply oldTypJT prevMutType newTypJT old_mem n'Of dly me meu = do
  -- (debug)
  str <- D.toList dly
  traceM "\nOld:"
  traceM $ pShowStrMut mut mutJT oldTypJT str
  traceM "\nNew:"
  traceM $ pShowStrMut mut mutJT newTypJT str
  traceM $ pShow me
  traceM $ pShow meu
  --

  -- cis --
  new_cis' <- case prevMutType of
    Add -> return $ CIs.join old_cis dCIs
    Del -> fst <$> CIs.difference dly (Just old_mem) Nothing old_cis dCIs

  return $ ME mut new_dnsLoss new_ddns new_dnm' new_cis'

  where
    ME mut old_dnsLoss old_ddns old_dnm old_cis@(CIs mutJT _ _ _) = me
    MEU n'Ils dCor dCIs@(CIs _ dcis_ns _ _) = meu

    -- ddns --
    new_ddns = IM.mergeWithKey (\_ (_, ddn') _ -> nothingIf (==0) ddn')
               (snd <$>) id ddnsIls old_ddns -- left-biased union
    ddnsIls = IM.mergeWithKey (\_ ddn d -> Just (ddn, ddn + d))
              (const IM.empty) ((0,) <$>) old_ddns dddns
    dddns = case Mut.typeOfMut mut of
      Add -> negate <$> udddns
      Del -> udddns
    udddns = imUnion dCIsCounts dCor
    dCIsCounts = case prevMutType of
      Add -> dcis_ns
      Del -> negate <$> dcis_ns

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
                   in logFact old_n'' - logFact new_n'' )
               n'Ils ddnsIls

    -- dnm --
    new_dnm' = old_dnm + dDnm
    dDnm = negate (sum dddns) & \r ->
      if even r then r `div` 2
      else err' $ "expected even number: " ++ show (r, dddns)
    err' = err . ("apply: " ++)

err :: String -> a
err = error . ("MutEntry.Update." ++)
