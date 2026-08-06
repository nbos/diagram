{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.MutBooks (module Diagram.Evolution.MutBooks) where

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import Data.Tuple.Extra
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Mutable as MV

import Diagram.Primitive

import Diagram.Evolution.Mutation (Mutation(..))
import Diagram.Evolution.MutEntry (MutEntry(..))

import Diagram.Util

-----------
-- BOOKS --
-----------

type MutBooksT m = StateT (MutBooks (PrimState m)) m
data MutBooks s = MutBooks
  -- mutType ------> dnm ------> dnsLoss --> mut ---> entry
  { _ixAddLeft  :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _ixAddRight :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _ixAdd2     :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _ixDelLeft  :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _ixDelRight :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _ixDel2     :: !(IntMap (Map Double (Map Mutation MutEntry)))
  , _byMut      :: !(Map Mutation MutEntry) -- by mutation
  , _byAffected :: !(MV.MVector s (Map Mutation ())) } -- by each sym in ddns
makeLenses ''MutBooks

empty :: PrimMonad m => Int -> m (MutBooks (PrimState m))
empty m = MutBooks IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
                   M.empty <$> MV.replicate m M.empty

fromList :: PrimMonad m => Int -> [MutEntry] -> m (MutBooks (PrimState m))
fromList m es = empty m >>= execStateT (mapM_ insert es)

-- | Insert an entry in the books
insert :: PrimMonad m => MutEntry -> MutBooksT m ()
insert e@(ME mut loss ddns dnm _) = do
  modify $ mutLens %~ IM.insertWith
    (\_ -> M.insertWith (\_ -> M.insert mut e) loss singleton0)
    dnm singleton1
  byMut %= M.insert mut e

  affected <- use byAffected
  forM_ (IM.keys ddns) $ MV.modify affected $ M.insert mut ()
  where
    singleton0 = M.singleton mut e
    singleton1 = M.singleton loss singleton0
    mutLens = case mut of
      AddLeft _  -> ixAddLeft
      AddRight _ -> ixAddRight
      Add2 _ _   -> ixAdd2
      DelLeft _  -> ixDelLeft
      DelRight _ -> ixDelRight
      Del2 _ _   -> ixDel2

-- | Delete an entry from the books. Assumes an entry in the books is
-- associated with that mutation.
delete :: PrimMonad m => Mutation -> MutBooksT m ()
delete mut = delete_ =<< byMut %%= findDelete mut

-- | Delete an entry in the index and affected vector. Doesn't delete
-- from the byMut map.
delete_ :: PrimMonad m => MutEntry -> MutBooksT m ()
delete_ (ME mut loss ddns dnm _) = do
  let f = nothingIf M.null . M.update g loss
      g = nothingIf M.null . M.delete mut
  modify $ mutLens %~ IM.update f dnm

  affected <- use byAffected
  forM_ (IM.keys ddns) $ MV.modify affected $ M.delete mut
  where
    mutLens = case mut of
      AddLeft _  -> ixAddLeft
      AddRight _ -> ixAddRight
      Add2 _ _   -> ixAdd2
      DelLeft _  -> ixDelLeft
      DelRight _ -> ixDelRight
      Del2 _ _   -> ixDel2

findDelete :: (Show k, Ord k) => k -> Map k a -> (a, Map k a)
findDelete k = first (fromMaybe err')
               . M.updateLookupWithKey (\_ _ -> Nothing) k
  where err' = err $ "findDelete: key not in map: " ++ show k

-- | Delete the old entry associated with the mutation of the given
-- entry and insert the new one. Assumes the entry's mutation had an
-- entry.
update :: PrimMonad m => MutEntry -> MutBooksT m ()
update e@(ME mut _ _ _ _) = delete mut >> insert e

err :: String -> a
err = error . ("MutBooks." ++)
