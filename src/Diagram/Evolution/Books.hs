{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.Books (module Diagram.Evolution.Books) where

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

import Diagram.Pretty (pShowStr)
import Diagram.Primitive

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

data Entry = E
  { _mutation        :: !Mutation
  , _ddSymCountsLoss :: !Double
  , _ddSymCounts     :: !(IntMap Int)
  , _dJointCount     :: !Int
  , _sites           :: !CIs }
  deriving (Show,Eq)
makeLenses ''Entry

fromParams :: JointType -> [Sym] -> (Sym -> Count) -> Mutation -> CIs -> Entry
fromParams = unflip5 fromParamsWith IM.empty

-- | Construct a mutation entry with a count correction
fromParamsWith :: JointType -> [Sym] -> -- TODO: rm JointType and [Sym] (DEBUG)
                  (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> Entry
fromParamsWith jt str n'Of mut cis cor = E mut loss ddns dnm cis
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
eval :: Int -> Int -> Int -> Int -> Entry -> Double
eval m bigN nm vm' (E mut dnsLoss _ dnm _)
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
err = error . ("Books." ++)

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
  , _byAffected :: !(MV.MVector s (Map Mutation ())) } -- by each sym in ddns
makeLenses ''Books

empty :: PrimMonad m => Int -> m (Books (PrimState m))
empty m = Books IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
                     M.empty <$> MV.replicate m M.empty

fromList :: PrimMonad m => Int -> [Entry] -> m (Books (PrimState m))
fromList m es = empty m >>= execStateT (mapM_ insert es)

-- | Insert an entry in the books
insert :: PrimMonad m => Entry -> BooksT m ()
insert e@(E mut loss ddns dnm _) = do
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
delete :: PrimMonad m => Mutation -> BooksT m ()
delete mut = delete_ =<< byMut %%= findDelete mut

-- | Delete an entry in the index and affected vector. Doesn't delete
-- from the byMut map.
delete_ :: PrimMonad m => Entry -> BooksT m ()
delete_ (E mut loss ddns dnm _) = do
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
update :: PrimMonad m => Entry -> BooksT m ()
update e@(E mut _ _ _ _) = delete mut >> insert e
