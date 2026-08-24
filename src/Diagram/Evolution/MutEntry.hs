{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.MutEntry (module Diagram.Evolution.MutEntry) where

import Control.Lens hiding (both,last1,Index,(:>),index)

import Data.Maybe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Pretty

import Diagram.String
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)

import qualified Diagram.Simple as Simple
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT

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

fromParams :: Monad m => JointType -> [Sym] ->
              (Sym -> m Count) -> Mutation -> CIs -> m MutEntry
fromParams = unflip5 fromParamsWith IM.empty

-- | Construct a mutation entry with a count correction
fromParamsWith :: Monad m => JointType -> [Sym] -> -- TODO: rm JointType and [Sym] (DEBUG)
                  (Sym -> m Count) -> Mutation -> CIs -> IntMap Int -> m MutEntry
fromParamsWith jt str n'Of mut cis cor = do

  losses <- sequence $ flip IM.mapWithKey ddns $ \s ddn -> do
    n' <- n'Of s
    let n'' = n' + ddn
        verif_n   = fromMaybe 0 $ IM.lookup s ns
        verif_n'  = fromMaybe 0 $ IM.lookup s ns'
        verif_n'' = fromMaybe 0 $ IM.lookup s ns''
    case () of
      _ | n' /= verif_n' -> err' $
          "Count before mut (n') is not what it should be\n"
          ++ "\nString (before):\n" ++ pShowStr mark False jt str ++ "\n\n"
          ++ "String (delta):\n"
          ++ pShowStr mark mutJtSign (cis^.CIs.jointType) str ++ "\n\n"
          ++ "String (after):\n" ++ pShowStr mark False jt' str ++ "\n\n"
          ++ "  mut: " ++ show mut ++ "\n"
          ++ "  sym: " ++ show s   ++ "\n"
          ++ "  n': "  ++ show n'  ++ "\n"
          ++ "  verif_n': " ++ show verif_n' ++ "\n"

        | n'' /= verif_n'' -> err' $
          "Count after mut (n'') is not what it should be\n"
          ++ "\nString (before):\n" ++ pShowStr mark False jt str ++ "\n\n"
          ++ "String (delta):\n"
          ++ pShowStr mark mutJtSign (cis^.CIs.jointType) str ++ "\n\n"
          ++ "String (after):\n" ++ pShowStr mark False jt' str ++ "\n\n"
          ++ "  mut: " ++ show mut ++ "\n"
          ++ "  sym: " ++ show s   ++ "\n"
          ++ "  n': "  ++ show n'  ++ "\n"
          ++ "  n'': " ++ show n''
          ++ " (n': " ++ show n'
          ++ ", cis: " ++ show (IM.lookup s mutSymCounts)
          ++ ", cor: " ++ show (IM.lookup s cor) ++ ")\n"
          ++ "  verif_n: "   ++ show verif_n   ++ "\n"
          ++ "  verif_n': "  ++ show verif_n'  ++ "\n"
          ++ "  verif_n'': " ++ show verif_n'' ++ "\n"
          ++ "  cis: " ++ show cis ++ "\n"

        | otherwise -> return $ logFact n' - logFact n''

  return $ ME mut (sum losses) ddns dnm cis

  where
    two_dnm = negate $ sum ddns
    dnm | odd two_dnm = err' $ "expected even number: " ++ show (two_dnm, ddns)
        | otherwise = two_dnm `div` 2
    mutSymCounts = cis^.CIs.symCounts
    mutType = typeOfMut mut
    signedMutSymCounts | mutType == Add = negate <$> mutSymCounts
                       | otherwise = mutSymCounts
    ddns = signedMutSymCounts `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

    -- verif -- TODO: remove
    ns    = Simple.symCounts str
    str'  = Simple.subst jt 256 str
    ns'   = Simple.symCounts str'
    jt'   = JT.appMut mut jt
    str'' = Simple.subst jt' 256 str
    ns''  = Simple.symCounts str''
    mutJtSign = mutType == Del
    mark = case mut of
      AddLeft s0  -> (== s0)
      AddRight s1 -> (== s1)
      Add2 s0 s1  -> (\s -> s == s0 || s == s1)
      DelLeft s0  -> (== s0)
      DelRight s1 -> (== s1)
      Del2 s0 s1  -> (\s -> s == s0 || s == s1)

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
