{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Diagram.Evolution.MutEntry.Update (
  module Diagram.Evolution.MutEntry.Update ) where

import Control.Lens

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.String
import Diagram.Primitive

import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math ( logFact )
import Diagram.Evolution.Mutation
import Diagram.Evolution.MutEntry (MutEntry(..))

import Diagram.Util

data Update = MEU
  { _onSymCounts :: !(IntMap (Count, Count, Double)) -- interval +
    -- deltaLoss precomputed because there is a lot of sharing
  , _onCorrection :: !(IntMap Int)
  , _addIntervals :: !CIs
  , _delIntervals :: !CIs }
  deriving (Show,Eq)
makeLenses ''Update

------------------
-- CONSTRUCTION --
------------------

empty :: Update
empty = MEU IM.empty IM.empty CIs.empty CIs.empty

fromDCounts :: IntMap (Count, Count, Double) -> Update
fromDCounts dnIls = empty{ _onSymCounts = dnIls }

fromDCor :: IntMap Int -> Update
fromDCor ddnIls = empty{ _onCorrection = ddnIls }

fromAddCIs :: CIs -> Update
fromAddCIs cis = empty{ _addIntervals = cis }

fromDelCIs :: CIs -> Update
fromDelCIs cis = empty{ _delIntervals = cis }

err :: String -> a
err = error . ("MutEntry.Update." ++)

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
apply :: PrimMonad m => (Sym -> Sym -> m Bool) -> (Sym -> Count) ->
  Doubly (PrimState m) -> Update -> MutEntry -> m MutEntry
apply old_mem n'Of dly (MEU n'Ils dCor addCIs delCIs) e = do
  -- cis --
  new_cis' <- CIs.join addCIs . fst <$>
    CIs.difference dly (Just old_mem) Nothing old_cis delCIs
  return $ ME mut new_dnsLoss new_ddns new_dnm' new_cis'

  where
    ME mut old_dnsLoss old_ddns old_dnm old_cis = e

    -- ddns --
    new_ddns = IM.mergeWithKey (\_ (_, ddn') _ -> nothingIf (==0) ddn')
               (snd <$>) id ddnsIls old_ddns -- left-biased union
    ddnsIls = IM.mergeWithKey (\_ ddn d -> Just (ddn, ddn + d))
              (const IM.empty) ((0,) <$>) old_ddns dddns
    dddns = case typeOfMut mut of
      Add -> negate <$> udddns
      Del -> udddns
    udddns = imUnion dCIsCounts dCor
    dCIsCounts = IM.mergeWithKey (const $ nothingIf (==0) .: (-))
                 id (negate <$>)
                 (addCIs^.CIs.symCounts) (delCIs^.CIs.symCounts)

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
