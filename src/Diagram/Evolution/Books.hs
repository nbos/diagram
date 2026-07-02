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

type BooksT m = StateT (Books (PrimState m)) m
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
fromList m es = empty m >>= execStateT (mapM_ insert es)

-- | Index the given entry in the given books by type, dnm, dnsLoss and
-- mut. Does nothing to byMut or byAffected.
index :: Entry -> Books s -> Books s
index e@(E mut loss _ dnm _) = ( case e^.eMut of
                 AddLeft _  -> ixAddLeft
                 AddRight _ -> ixAddRight
                 Add2 _ _   -> ixAdd2
                 DelLeft _  -> ixDelLeft
                 DelRight _ -> ixDelRight
                 Del2 _ _   -> ixDel2 ) %~ go
  where
    singleton0 = M.singleton mut e
    singleton1 = M.singleton loss singleton0
    go = IM.insertWith
         (\_ -> M.insertWith (\_ -> M.insert mut e) loss singleton0)
         dnm singleton1

-- | De-index the given entry in the given books by type, dnm, dnsLoss
-- and mut. Does nothing to byMut or byAffected.
deIndex :: Entry -> Books s -> Books s
deIndex e@(E mut loss _ dnm _) = ( case e^.eMut of
                 AddLeft _  -> ixAddLeft
                 AddRight _ -> ixAddRight
                 Add2 _ _   -> ixAdd2
                 DelLeft _  -> ixDelLeft
                 DelRight _ -> ixDelRight
                 Del2 _ _   -> ixDel2 ) %~ go
  where
    go = flip IM.update dnm $
         (nothingIf M.null .) $ flip M.update loss $
         nothingIf M.null . M.delete mut

-- | Insert an entry in the books
insert :: PrimMonad m => Entry -> BooksT m ()
insert e@(E mut _ dns _ _) = do
  mv <- use byAffected
  modify $ index e
  byMut %= M.insert mut e
  forM_ (IM.keys dns) $ MV.modify mv $ M.insert mut ()

-- | Delete an entry from the books
delete :: PrimMonad m => Entry -> BooksT m ()
delete e@(E mut _ dns _ _) = do
  mv <- use byAffected
  modify $ deIndex e
  byMut %= M.delete mut
  forM_ (IM.keys dns) $ MV.modify mv $ M.delete mut

-- | Delete the first entry and insert the second
update :: PrimMonad m => Entry -> Entry -> BooksT m ()
update old new = delete old >> insert new

err :: String -> a
err = error . ("Books." ++)
