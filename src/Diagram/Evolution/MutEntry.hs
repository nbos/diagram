{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Evolution.MutEntry (module Diagram.Evolution.MutEntry) where

import Control.Lens hiding (both,last1,Index,(:>),index)

import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Pretty
import Diagram.String
import qualified Diagram.Simple as Simple
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)

import Diagram.Util

--------------------
-- MUTATION ENTRY --
--------------------

data MutEntry = ME
  { _mutation        :: !Mutation     -- mut
  , _ddSymCountsLoss :: !Double       -- ddnsLoss
  , _ddSymCounts     :: !(IntMap Int) -- ddns
  , _dJointCount     :: !Int          -- dnm (ddnm)
  , _sites           :: !CIs }        -- cis
  deriving (Show,Eq)
makeLenses ''MutEntry

-- | Evaluate full loss given parameters
eval :: Int -> Int -> Int -> Int -> MutEntry -> Double
eval m bigN nm vm' (ME mut dnsLoss _ dnm _)
  | isInfinite res = err' $ "Books.eval: infinite loss: "
                     ++ "m=" ++ show m
                     ++ ", bigN=" ++ show bigN
                     ++ ", nm=" ++ show nm
                     ++ ", vm'=" ++ show vm'
                     ++ ", mut=" ++ show mut
                     ++ ", dnsLoss=" ++ show dnsLoss
                     ++ ", dnm=" ++ show dnm
                     ++ ", dnmLoss=" ++ show dnmLoss
  | otherwise = res
  where
    res = dnsLoss + dnmLoss
    dnmLoss = Math.dnmLoss m bigN nm vm' dnm
    err' = err . ("eval: " ++)

------------------
-- CONSTRUCTION --
------------------

-- | Construct a mutation entry that needs no count correction.
fromParams :: (Sym -> Count) -> Mutation -> CIs -> MutEntry
fromParams n'Of mut cis = fromParamsWith_ n'Of mut cis ddns
  where mutSymCounts = cis^.CIs.symCounts
        ddns = case typeOfMut mut of
          Add -> negate <$> mutSymCounts
          Del -> mutSymCounts

-- | Construct a mutation entry with a count correction.
fromParamsWith :: (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> MutEntry
fromParamsWith n'Of mut cis@(CIs _ mutSymCounts _ _) cor =
  fromParamsWith_ n'Of mut cis ddns
  where
    ddns = case typeOfMut mut of
      Add -> negate <$> corMutSymCounts
      Del -> corMutSymCounts
    corMutSymCounts = mutSymCounts `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

-- | Construct a mutation entry given the delta delta sym count (ddns).
fromParamsWith_ :: (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> MutEntry
fromParamsWith_ n'Of mut cis ddns =
  ME mut loss ddns dnm cis
  where
    two_dnm = negate $ sum ddns
    dnm = two_dnm `div` 2
    loss = sum $ flip IM.mapWithKey ddns $
      \s ddn -> let n' = n'Of s
                    n'' = n' + ddn
                in logFact n' - logFact n''

validate :: JointType -> [Sym] -> (Sym -> Count) -> MutEntry -> MutEntry
validate jt str n'Of e@(ME mut loss ddns dnm (CIs mutJT mutCounts _ _))
  | odd two_dnm = err' $
    "Expected even number: " ++ show (two_dnm, ddns) ++ "\n"
    ++ "\nString (before):\n" ++ pShowStr jt    str ++ "\n\n"
    ++ "String (delta):\n"    ++ pShowStr mutJT str ++ "\n\n"
    ++ "String (after):\n"    ++ pShowStr jt'   str ++ "\n\n"
    ++ "  mut: " ++ show mut ++ "\n"
    ++ "  ddns (cis + cor): " ++ show ddns ++ "\n"
  | dnm /= (two_dnm `div` 2) = err' $
    "Delta joint count doesn't match with sum of delta symbol counts: "
    ++ show (dnm, two_dnm `div` 2) ++ "\n"
    ++ "\nString (before):\n" ++ pShowStr jt    str ++ "\n\n"
    ++ "String (delta):\n"    ++ pShowStr mutJT str ++ "\n\n"
    ++ "String (after):\n"    ++ pShowStr jt'   str ++ "\n\n"
    ++ "  mut: " ++ show mut ++ "\n"
    ++ "  ddns (cis + cor): " ++ show ddns ++ "\n"
  | rel_err_loss > 0.001 && err_loss > 1 = err' $
    "Error on loss is not negligible: "
    ++ "  Entry loss: "     ++ pShow loss         ++ "\n"
    ++ "  Actual loss: "    ++ pShow verif_loss   ++ "\n"
    ++ "  Absolute error: " ++ pShow err_loss     ++ "\n"
    ++ "  Relative error: " ++ pShow rel_err_loss ++ "\n"
  | otherwise = e
  where
    two_dnm = negate $ sum ddns
    verif_loss = sum losses
    err_loss = abs $ verif_loss - loss
    rel_err_loss = err_loss / loss
    losses = flip IM.mapWithKey ddns $ \s ddn -> do
      let n' = n'Of s
          n'' = n' + ddn
          verif_n   = fromMaybe 0 $ IM.lookup s ns
          verif_n'  = fromMaybe 0 $ IM.lookup s ns'
          verif_n'' = fromMaybe 0 $ IM.lookup s ns''
      case () of
        _ | n' /= verif_n' -> err' $
            "Count before mut (n') is not what it should be\n"
            ++ "\nString (before):\n" ++ pShowStrMark (==s) jt    str ++ "\n\n"
            ++ "String (delta):\n"    ++ pShowStrMark (==s) mutJT str ++ "\n\n"
            ++ "String (after):\n"    ++ pShowStrMark (==s) jt'   str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  verif_n': " ++ show verif_n' ++ "\n"

          | n'' /= verif_n'' -> err' $
            "Count after mut (n'') is not what it should be\n"
            ++ "\nString (before):\n" ++ pShowStrMark (==s) jt    str ++ "\n\n"
            ++ "String (delta):\n"    ++ pShowStrMark (==s) mutJT str ++ "\n\n"
            ++ "String (after):\n"    ++ pShowStrMark (==s) jt'   str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  n'': " ++ show n''
            ++ " (n': " ++ show n' ++ ", ddns: " ++ show ddn
            ++ " (cis: " ++ show (IM.lookup s mutCounts)
            ++ ", cor: " ++ show (ddn - fromMaybe 0 (mutCounts IM.!? s)) ++ "))\n"
            ++ "  verif_n: "   ++ show verif_n   ++ "\n"
            ++ "  verif_n': "  ++ show verif_n'  ++ "\n"
            ++ "  verif_n'': " ++ show verif_n'' ++ "\n"

          | otherwise -> logFact n' - logFact n''

    ns    = Simple.symCounts str
    str'  = Simple.subst jt 256 str
    ns'   = Simple.symCounts str'
    jt'   = JT.appMut mut jt
    str'' = Simple.subst jt' 256 str
    ns''  = Simple.symCounts str''
    err' = err . ("validate: " ++)

err :: String -> a
err = error . ("MutEntry." ++)
