{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution.Books (module Diagram.Evolution.Books) where

import Control.Monad
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import Data.Maybe
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

import Diagram.Simple
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT

import Diagram.Util

--------------------
-- MUTATION ENTRY --
--------------------

data Entry = E
  { _mutation        :: !Mutation
  , _d2SymCountsLoss :: !Double
  , _d2SymCounts     :: !(IntMap Int)
  , _deltaJointCount :: !Int
  , _sites           :: !CIs }
  deriving (Show,Eq)
makeLenses ''Entry

fromParams :: JointType -> [Sym] -> (Sym -> Count) -> Mutation -> CIs -> Entry
fromParams = unflip5 fromParamsWith IM.empty

-- | Construct a mutation entry with a count correction
fromParamsWith :: JointType -> [Sym] ->
                  (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> Entry
fromParamsWith jt@(JT u0 u1) str n'Of mut cis cor = E mut loss ddns dnm cis
  where
    loss = sum $ flip IM.mapWithKey ddns $ \s ddn ->
      let n = fromMaybe 0 $ IM.lookup s ns
          n' = n'Of s
          verif_n' = fromMaybe 0 $ IM.lookup s ns'
          n'' = n' + ddn
          verif_n'' = fromMaybe 0 $ IM.lookup s ns''
      in case () of
        _ | n' /= verif_n' -> error $
            "Count before mut (n') is not what it should be\n"
            ++ "\nString:\n" ++ pprint str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  verif_n': " ++ show verif_n' ++ "\n"

          | n'' /= verif_n'' -> error $
            "Count after mut (n'') is not what it should be\n"
            ++ "\nString:\n" ++ pprint str ++ "\n\n"
            ++ "  mut: " ++ show mut ++ "\n"
            ++ "  sym: " ++ show s   ++ "\n"
            ++ "  n: "   ++ show n   ++ "\n"
            ++ "  n': "  ++ show n'  ++ "\n"
            ++ "  n'': " ++ show n''
            ++ " (cis: " ++ show (IM.lookup s sns)
            ++ ", cor: " ++ show (IM.lookup s cor) ++ ")\n"
            ++ "  verif_n'': " ++ show verif_n'' ++ "\n"

          | otherwise ->  logFact n' - logFact n''

    dnm = -(sum ddns `div` 2)
    sns = cis^.CIs.symCounts
    ssns | typeOfMut mut == Add = negate <$> sns
         | otherwise = sns
    ddns = ssns `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

    -- verif
    ns = symCounts str
    str' = subst jt 256 str
    ns' = symCounts str'
    jt' = JT.appMut mut jt
    str'' = subst jt' 256 str
    ns'' = symCounts str''

    pprint [] = normal
    pprint [s] = red ++ show s ++ normal
    pprint (s0:s1:ss)
      | mem s0 s1 =
        (if not (mem' s0 s1) then normal ++ "***" else "") -- del i0
        ++ normal ++ "(" ++ green ++ show s0 ++ " "
        ++ (case ss of s2:_ | not (mem s1 s2)
                            , mem' s1 s2 -> normal ++ "***" ++ green -- add i1
                       _else -> "")
        ++ show s1 ++ normal ++ ") "
        ++ pprint ss

      | mem' s0 s1 = normal ++ "***" ++ red ++ show s0 ++ " " ++ pprint (s1:ss) -- add i0
      | otherwise = red ++ show s0 ++ " "
                    ++ pprint (s1:ss)
      where
        mem = (`JT.member` jt) .: (,)
        mem' = (`JT.member` jt') .: (,)

    red = "\ESC[91m"
    green = "\ESC[32m"
    normal = "\ESC[0m"

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
  , _byAffected :: !(MV.MVector s (Map Mutation ())) } -- by each sym in ddns
makeLenses ''Books

empty :: PrimMonad m => Int -> m (Books (PrimState m))
empty m = Books IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
                     M.empty <$> MV.replicate m M.empty

fromList :: PrimMonad m => Int -> [Entry] -> m (Books (PrimState m))
fromList m es = empty m >>= execStateT (mapM_ insert es)

-- | Index the given entry in the given books by type, dnm, dnsLoss and
-- mut. Does nothing to byMut or byAffected.
index :: Entry -> Books s -> Books s
index e@(E mut loss _ dnm _) =
  ( case e^.mutation of
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
deIndex e@(E mut loss _ dnm _) = ( case e^.mutation of
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
insert e@(E mut _ ddns _ _) = do
  mv <- use byAffected
  modify $ index e
  byMut %= M.insert mut e
  forM_ (IM.keys ddns) $ MV.modify mv $ M.insert mut ()

-- | Delete an entry from the books
delete :: PrimMonad m => Entry -> BooksT m ()
delete e@(E mut _ ddns _ _) = do
  mv <- use byAffected
  modify $ deIndex e
  byMut %= M.delete mut
  forM_ (IM.keys ddns) $ MV.modify mv $ M.delete mut

-- | Delete the first entry and insert the second
update :: PrimMonad m => Entry -> Entry -> BooksT m ()
update old new = delete old >> insert new

err :: String -> a
err = error . ("Books." ++)
