{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Diagram.Evolution (module Diagram.Evolution) where

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as U


import Diagram.Primitive

import Diagram.Joints (Joints)
import Diagram.JointType (JointType)
import Diagram.String
import Diagram.ConstrInterval(CI(..), ciLength, tailSymbol, tailIndex)
import qualified Diagram.ConstrInterval as CI
import Diagram.ConstrIntervals (CIs(..))
import qualified Diagram.ConstrIntervals as CIs

import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)

import Diagram.Evolution.TypeState (TypeState(TS))
import qualified Diagram.Evolution.TypeState as TS

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

mkEntry :: (Sym -> Count) -> Mutation -> CIs -> Entry
mkEntry nOf mut cis = mkEntryWith nOf mut cis IM.empty

-- | Construct a mutation entry with a count correction
mkEntryWith :: (Sym -> Count) -> Mutation -> CIs -> IntMap Int -> Entry
mkEntryWith nOf mut cis cor = E mut (Math.dnsLoss ils) dns dnm cis
  where
    ils = (<$> IM.toList dns) $ \(s,dn) -> toSnd (+dn) $ nOf s
    dnm = -(sum dns `div` 2)
    ns = cis^.CIs.symCounts
    dns | Add <- typeOfMut mut = (negate <$> ns) `union` cor
        | otherwise = ns `union` cor
    union = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id

evalEntry :: Int -> Int -> Int -> Int -> Entry -> Double
evalEntry m bigN nm vm' (E _ dnsLoss _ dnm _) = dnsLoss + dnmLoss
  where dnmLoss = Math.dnmLoss m bigN nm vm' dnm

-----------
-- BOOKS --
-----------

data Books = Books
  -- mutType ------> dnm ------> dnsLoss --> mut ---> entry
  { _ixAddLeft  :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixAddRight :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixAdd2     :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDelLeft  :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDelRight :: !(IntMap (Map Double (Map Mutation Entry)))
  , _ixDel2     :: !(IntMap (Map Double (Map Mutation Entry))) }
makeLenses ''Books

emptyBooks :: Books
emptyBooks = Books IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty

mkBooks :: [Entry] -> Books
mkBooks es = runIdentity $ flip execStateT emptyBooks $
  forM_ es $ \e ->
  ( case e^.eMut of AddLeft _  -> ixAddLeft
                    AddRight _ -> ixAddRight
                    Add2 _ _   -> ixAdd2
                    DelLeft _  -> ixDelLeft
                    DelRight _ -> ixDelRight
                    Del2 _ _   -> ixDel2 ) %= insert e
  where
    insert e = IM.insertWith (M.unionWith (M.unionWith err')) (e^.eDnm) $
               M.singleton (e^.eDnsLoss) (M.singleton (e^.eMut) e)
    err' = err . ("mkBooks: collision: " ++) . show .: (,)

err :: String -> a
err = error . ("Evolution." ++)

----------------------
-- EVOLUTION STATE  --
----------------------

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
-- | Evolution state of a JointType in a given string
data EvolutionState s = EvolutionState
  -- String state (readonly)
  { _stringLen :: !Int -- N, bigN
  , _doubly :: !(Doubly s) -- dly :: underlying string :: [N]Sym
  , _symCounts :: !(U.Vector Count) -- ns :: symbol counts (TODO: dyn?)

  -- Type intro state
  , _typeState :: !(TypeState s)
  , _symDeltas :: !(IntMap Int) -- dns :: delta symbol count :: u0 U u1 -> dn
  , _jointCount :: !Count -- nm :: joint count, popCount of constructed

  -- Books
  , _mutBooks :: !Books }
makeLenses ''EvolutionState

-- GETTERS --

-- | m
numSymbols :: Monad m => EvolutionT m Int
numSymbols = zoom typeState TS.numSymbols

-- | vm = sz0 * sz1
variety :: Monad m => EvolutionT m Int
variety = zoom typeState TS.variety

-- | Compute the difference in information/code length incurred by the
-- introduction of the current joint type (i.e. no further mutation)
getIntroInfo :: Monad m => EvolutionT m Double
getIntroInfo = Math.dInfo <$> numSymbols -- m
                          <*> use stringLen -- N
                          <*> use jointCount -- nm
                          <*> (d2ils <*> use symDeltas) -- ils
                          <*> variety -- vm
  where d2ils = (<$> use symCounts) $
          \ns -> IM.elems . IM.mapWithKey (\s dn -> toSnd (+dn) (ns U.! s))

-- EVAL --

-- | Enumerate all available mutations with their loss. Unsorted.
evalAll :: Monad m => EvolutionT m [(Double, Entry)]
evalAll = evalAll_ <$> numSymbols -- m
                   <*> use stringLen -- N
                   <*> use jointCount -- nm
                   <*> use typeState -- TypeState
                   <*> use mutBooks -- Books

evalAll_ :: Int -> Int -> Int -> TypeState s -> Books -> [(Double, Entry)]
evalAll_ m bigN nm (TS sz0 _ sz1 _) (Books als ars a2s dls drs d2s) =
  concat $ zipWith (fmap . toFst) lossFns entries
  where
    vm = sz0 * sz1
    lossFns :: [Entry -> Double]
    lossFns = evalEntry m bigN nm <$> vm's
    vm's = [ vm + sz1 -- addLeft
           , vm + sz0 -- addRight
           , vm + sz0 + sz1 -- add2
           , vm - sz1 -- delLeft
           , vm - sz0 -- delRight
           , vm - sz0 - sz1 ] :: [Int] -- del2

    entries :: [[Entry]]
    entries = flatten <$> [ als, ars, a2s, dls, drs, d2s ]
    flatten = concatMap (concatMap M.elems . M.elems)
              . IM.elems

----------
-- INIT --
----------

init :: forall m. PrimMonad m =>
  Int -> Int -> Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
  (JointType, Joints CIs) -> m (EvolutionState (PrimState m))
init m bigN dly ns jointCIs (jt, memJointCIs) = do
  tst <- TS.init m allJoints jt

  cisByMut <- joinByMut tst CIs.join $ M.toList jointCIs
  corrsByMut <- M.unionsWith (IM.unionWith (+))
                <$> mapM (corrections dly tst) (CIs.toList memCIs)

  return $ EvolutionState bigN dly ns tst dns nm $ mkBooks $ M.elems $
    M.mergeWithKey (Just .:. mkEntryWith nOf) -- both CIs + corr
    (M.mapWithKey $ mkEntry nOf) -- only CIs
    (error . ("CIs missing: " ++) . show) -- only corr
    cisByMut corrsByMut

  where
    allJoints = M.keys jointCIs
    nOf s = maybe n (+n) $ IM.lookup s dns
      where n = ns U.! s

    memCIs = foldr1 CIs.join memJointCIs
    ndns = memCIs^.CIs.symCounts -- negative delta symbol counts

    nm = sum ndns `div` 2 -- nm := d1nm because d0nm == 0
    dns = negate <$> ndns -- delta symbol counts (intro's)

-- WHERE --

-- | Combine values keyed by joints flipped (in/out) by the same
-- mutation together, given a combining function
joinByMut :: forall m a. PrimMonad m => TypeState (PrimState m) ->
  (a -> a -> a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMut tst f = fmap (M.fromListWith f . concat) . mapM g
  where
    g :: ((Sym,Sym), a) -> m [(Mutation, a)]
    g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

-- | Given the string, a type, its constructive signal on the string
-- (bool vector), and a constructive interval of the joint type, return
-- the set of corrections on the symCounts of each CIs associated with a
-- mutation (add or del) (all at once) required to be added in order for
-- it to match the actual change in symbol counts produced by the
-- mutation. Corrections are signed to be *added* to the CIs.symCounts
-- before they are subtracted (add) or added (del) to the joint type's
-- own CIs.symCounts.
corrections :: forall m. PrimMonad m => Doubly (PrimState m) ->
               TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
corrections dly tst ci = fmap clean $ do
  -- [DEL]: decompose, treat all delMuts
  dns' <- delCorrections dly tst ci

  -- [ADD]: grab the largest chain possible, if CI is first in the chain
  flip execStateT dns' $ (prevCI ci >>=) $ flip whenJust $ \case
    Nothing -> (nextCIs ci >>=) $ flip whenJust $
               \(addMut, nexts) -> insert addMut $ addCorrections (ci:|nexts)
    Just (addMut, prv) -> (nextCIs ci >>=) $ \case
      Nothing -> insert addMut $ addCorrections (prv:|[ci])
      Just (addMut', nexts)
        | addMut == addMut' -> insert addMut $ addCorrections (prv:|ci:nexts)
        | otherwise -> insert addMut (addCorrections (prv:|[ci]))
                       >> insert addMut' (addCorrections (ci:|nexts))
  where
    clean = M.filter IM.null . fmap (IM.filter (==0))

    prevCI = lift . TS.prevMutCI dly tst
    nextCIs = lift . TS.nextMutCIs dly tst

    insert :: Mutation -> IntMap Int -> StateT (Map Mutation (IntMap Int)) m ()
    insert mut im = modify $ M.insertWith (IM.unionWith (+)) mut im

-- | Given a non-empty list of overlapping (connecting) intervals after
-- an add mutation (alternating [in-]add-in-add-etc.), return the
-- appropriate corrections on delta delta symbol counts (ddns)
addCorrections :: NonEmpty CI -> IntMap Int
addCorrections ils = case compare (even newLen) (CI.even last_) of
  LT -> IM.insertWith (+) (last_^.tailSymbol) (-1) im
  EQ -> im
  GT -> IM.insertWith (+) (last_^.tailSymbol) 1 im
  where
    newLen = sum ((^.ciLength) <$> ils) -- constituents lengths
             - (length ils - 1) -- overlaps

    im = L.foldl' (flip f) IM.empty (NE.init ils)
    f ci | CI.even ci = IM.insertWith (+) (ci^.tailSymbol) (-1)
         | otherwise = id

    last_ = NE.last ils

-- | Given a constructive interval of the joint type (in), count all
-- the differences in symbol counts between the symCounts of the CIs
-- for all joints removed by the same del-mutation and and the real
-- difference in symCounts from applying those mutations.
delCorrections :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Map Mutation (IntMap Int))
delCorrections dly tst ci = do

  constr <- flip IS.member . IS.fromList . everyOther . fmap fst
            <$> CI.extension dly ci

  let go :: Mutation -> Bool -> [CI] -> StateT (Map Mutation (IntMap Int)) m ()
      go delMut = go_ where
        go_ _ [] = return ()
        go_ phase (CI hd shd len tl stl : rest) = do
          unless (tl == (ci^.tailIndex)) $ dec stl -- tl
          let outOfPhase = phase /= constr hd
          -- out of phase with super-CI means prev hd will be constr
          -- means hd will still be constr. so hd will not be docked
          when outOfPhase $ dec shd -- hd
          let phase' = phase /= odd len -- xor
          go_ phase' rest

        dec :: Sym -> StateT (Map Mutation (IntMap Int)) m ()
        dec s = modify $ M.insertWith (const $ IM.insertWith (+) s (-1))
                delMut (IM.singleton s (-1))

  flip execStateT M.empty $
    mapM_ (uc $ flip go True) -- True == constr
    . M.toList . M.fromListWith (++)
    . reverse . ffmap (:[]) -- reverse to maintain order
    =<< lift (TS.decomposeIn dly tst ci)

  where
    everyOther :: [a] -> [a]
    everyOther [] = []
    everyOther [a] = [a]
    everyOther (a:_:rest) = a : everyOther rest
