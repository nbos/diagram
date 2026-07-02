{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.Books (module Diagram.Evolution.Books) where

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import Data.Tuple.Extra (both)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Mutable as MV

import Diagram.Primitive

import Diagram.String
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)


import Diagram.Util

--------------------
-- MUTATION ENTRY --
--------------------

data Entry = E
  { _eMut :: !Mutation
  , _eDnsLoss :: !Double
  , _eDns :: !(IntMap Int)
  , _eDnm :: !Int
  , _eCIs :: !CIs }
  deriving (Show,Eq)
makeLenses ''Entry

fromParams :: (Sym -> Count) -> Mutation -> CIs -> Entry
fromParams n'Of mut cis = fromParamsWith n'Of mut cis IM.empty

-- | Construct a mutation entry with a count correction
fromParamsWith :: (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> Entry
fromParamsWith n'Of mut cis cor = E mut loss dns dnm cis
  where
    loss = sum $ uncurry (-) . both logFact <$> ils
    ils = (<$> IM.toList dns) $ \(s,dn) -> let n' = n'Of s
                                               n'' = n' + dn
                                           in seq n'' (n', n'')
    dnm = -(sum dns `div` 2)
    ns = cis^.CIs.symCounts
    dns = (if typeOfMut mut == Add then negate <$> ns else ns)
          `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

eval :: Int -> Int -> Int -> Int -> Entry -> Double
eval m bigN nm vm' (E _ dnsLoss _ dnm _) = dnsLoss + dnmLoss
  where dnmLoss = Math.dnmLoss m bigN nm vm' dnm

-----------
-- BOOKS --
-----------

data Books s = Books
  -- mutType ------> dnm ------> dnsLoss --> mut ---> entry
  { _ixAddLeft  :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixAddRight :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixAdd2     :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDelLeft  :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDelRight :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDel2     :: !(IntMap (Map Double (Map Mutation Entry)))
  , _byMut      :: !(Map Mutation Entry) -- by mutation
  , _byAffected :: !(MV.MVector s (Map Mutation ())) } -- by each sym in dns
makeLenses ''Books

empty :: PrimMonad m => Int -> m (Books (PrimState m))
empty m = Books IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
                     M.empty <$> MV.replicate m M.empty

fromList :: PrimMonad m => Int -> [Entry] -> m (Books (PrimState m))
fromList m es = (empty m >>=) $ execStateT $ do
  mv <- use byAffected
  forM_ es $ \e -> do
    let mut = e^.eMut
    ( case mut of AddLeft _  -> ixAddLeft
                  AddRight _ -> ixAddRight
                  Add2 _ _   -> ixAdd2
                  DelLeft _  -> ixDelLeft
                  DelRight _ -> ixDelRight
                  Del2 _ _   -> ixDel2 ) %= index e

    byMut %= M.insertWith err' mut e
    forM_ (IM.keys $ e^.eDns) $ MV.modify mv $ M.insertWith err' mut ()

  where
    index e = IM.insertWith (M.unionWith (M.unionWith err')) (e^.eDnm) $
               M.singleton (e^.eDnsLoss) (M.singleton (e^.eMut) e)

    err' :: (Show a, Show b) => a -> b -> c
    err' = err . ("mkBooks: collision: " ++) . show .: (,)

err :: String -> a
err = error . ("Books." ++)
