{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.MutEntry (module Diagram.Evolution.MutEntry) where

import Control.Lens hiding (both,last1,Index,(:>),index)

import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Pretty (pShowStr)

import Diagram.String
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)

import Diagram.Simple
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT

import Diagram.Util

--------------------
-- MUTATION ENTRY --
--------------------

data MutEntry = ME
  { _mutation        :: !Mutation
  , _ddSymCountsLoss :: !Double
  , _ddSymCounts     :: !(IntMap Int)
  , _dJointCount     :: !Int
  , _sites           :: !CIs }
  deriving (Show,Eq)
makeLenses ''MutEntry

fromParams :: JointType -> [Sym] -> (Sym -> Count) -> Mutation -> CIs -> MutEntry
fromParams = unflip5 fromParamsWith IM.empty

-- | Construct a mutation entry with a count correction
fromParamsWith :: JointType -> [Sym] -> -- TODO: rm JointType and [Sym] (DEBUG)
                  (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> MutEntry
fromParamsWith jt str n'Of mut cis cor = ME mut loss ddns dnm cis
  where
    loss = sum $ flip IM.mapWithKey ddns $ \s ddn ->
      let n = fromMaybe 0 $ IM.lookup s ns
          n' = n'Of s
          verif_n' = fromMaybe 0 $ IM.lookup s ns'
          n'' = n' + ddn
          verif_n'' = fromMaybe 0 $ IM.lookup s ns''
      in case () of
        _ | n' /= verif_n' -> err' $
            "Count before mut (n') is not what it should be\n"
            ++ "\nString:\n" ++ pShowStr jt jt' str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  verif_n': " ++ show verif_n' ++ "\n"

          | n'' /= verif_n'' -> err' $
            "Count after mut (n'') is not what it should be\n"
            ++ "\nString:\n" ++ pShowStr jt jt' str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n: "   ++ show n   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  n'': " ++ show n''
            ++ " (cis: " ++ show (IM.lookup s sns)
            ++ ", cor: " ++ show (IM.lookup s cor) ++ ")\n"
            ++ "  verif_n'': " ++ show verif_n'' ++ "\n"

          | otherwise -> logFact n' - logFact n''

    two_dnm = sum ddns
    dnm | odd two_dnm = err' $ "expected even number: " ++ show (two_dnm, ddns)
        | otherwise = -(two_dnm `div` 2)
    sns = cis^.CIs.symCounts
    ssns | typeOfMut mut == Add = negate <$> sns
         | otherwise = sns
    ddns = ssns `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

    -- verif -- TODO: remove
    ns = symCounts str
    str' = subst jt 256 str
    ns' = symCounts str'
    jt' = JT.appMut mut jt
    str'' = subst jt' 256 str
    ns'' = symCounts str''

    err' = err . ("fromParamsWith: " ++)

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

err :: String -> a
err = error . ("MutEntry." ++)
