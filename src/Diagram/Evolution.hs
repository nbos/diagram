{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Evolution (module Diagram.Evolution) where

import Prelude hiding (init)
import Debug.Trace

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import Data.Maybe
import Data.Function
import qualified Data.List as L

import qualified Data.Set as Set
import qualified Data.IntSet as IS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Mutable as MV

import Diagram.Pretty
import Diagram.Primitive

import Diagram.Joints (Joints)
import Diagram.UnionType (UnionType(UT))
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import Diagram.String
import Diagram.ConstrIntervals (CIs(CIs))
import qualified Diagram.ConstrIntervals as CIs
import qualified Diagram.Doubly as D

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)

import Diagram.Evolution.Correction (corrsOf)
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution.MutEntry (MutEntry(..), ddSymCounts, ddSymCountsLoss)
import qualified Diagram.Evolution.MutEntry as ME
import Diagram.Evolution.MutBooks (MutBooks(MutBooks), byAffected, byMut)
import qualified Diagram.Evolution.MutBooks as MB

import Diagram.Util

----------------------
-- EVOLUTION STATE  --
----------------------

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
-- | Evolution state of a JointType in a given string
data EvolutionState s = EvolutionState
  -- String state (readonly, only changes accross intros, not evolution)
  { _stringLen :: !Int -- N, bigN
  , _doubly :: !(Doubly s) -- dly :: underlying string :: [N]Sym
  , _symCounts :: !(U.Vector Count) -- ns :: symbol counts (TODO: dyn vec)
  , _jointCIs :: !(Joints CIs) -- allCIs :: (s0,s1) -> CIs

  -- curr. Type state (evolves)
  , _typeState :: !(TypeState s) -- sym entries :: [(mem, coIn, deps, coOut)]
  , _deltaCounts :: !(MU.MVector s Int) -- dns :: delta symbol counts
  , _jointCount :: !Count -- nm :: joint count, popCount of constructed

  -- indexed Mutations
  , _mutBooks :: !(MutBooks s) }
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
getIntroInfo :: PrimMonad m => EvolutionT m Double
getIntroInfo = Math.dInfo <$> numSymbols -- m
                          <*> use stringLen -- N
                          <*> use jointCount -- nm
                          <*> getCountIntervals -- [(n,n')]
                          <*> variety -- vm

getCountIntervals :: PrimMonad m => EvolutionT m [(Count,Count)]
getCountIntervals = snd <<$>> getSymCountIntervals

getSymCountIntervals :: PrimMonad m => EvolutionT m [(Sym,(Count,Count))]
getSymCountIntervals = do
  ns <- use symCounts
  dns <- use deltaCounts
  JT (UT _ s0s) (UT _ s1s) <- use $ typeState.TS.jointType
  forM (IS.toList $ IS.union s0s s1s) $ \s -> do -- for (u0 U u1)
    let n = ns U.! s
    dn <- MU.read dns s
    let n' = n + dn
    seq n' $ return (s,(n,n'))

-- EVAL --

-- | Enumerate all available mutations with their loss. Unsorted.
evalAll :: Monad m => EvolutionT m [(Double, MutEntry)]
evalAll = evalAll_ <$> numSymbols -- m
                   <*> use stringLen -- N
                   <*> use jointCount -- nm
                   <*> use typeState -- TypeState
                   <*> use mutBooks -- Books

evalAll_ :: Int -> Int -> Int -> TypeState s -> MutBooks s ->
            [(Double, MutEntry)]
evalAll_ m bigN nm tst (MutBooks als ars a2s dls drs d2s _ _) =
  concat $ zipWith (fmap . toFst) lossFns entries
  where
    (sz0,sz1) = JT.dims $ tst^.TS.jointType
    vm = sz0 * sz1
    lossFns :: [MutEntry -> Double]
    lossFns = ME.eval m bigN nm <$> vm's
    vm's = [ vm + sz1 -- addLeft
           , vm + sz0 -- addRight
           , vm + sz0 + sz1 + 1 -- add2
           , vm - sz1 -- delLeft
           , vm - sz0 -- delRight
           , vm - sz0 - sz1 + 1 ] :: [Int] -- del2

    entries :: [[MutEntry]]
    entries = flatten <$> [ als, ars, a2s, dls, drs, d2s ]
    flatten = concatMap (concatMap M.elems . M.elems)
              . IM.elems

-- EXEC --

-- | Hill climb to a local minimum, given parameters m (number of
-- symbols), N (string length), the string, symbol counts, constructive
-- intervals of all joints in the string and a joint type with its CIs
-- indexed by joint.
hillClimb :: forall m. PrimMonad m =>
  Int -> Int -> Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
  (JointType, Joints CIs) -> m JointType
hillClimb = init_ >======> execStateT (whileM step)
            >.> fmap (^.typeState.TS.jointType)

step :: PrimMonad m => EvolutionT m Bool
step = do
  es <- evalAll
  traceM "\nEntries:"
  mapM_ (traceM . pShow) es
  let (_, e) = L.minimumBy (compare `on` fst) es
  ddInfo <- ddInformation e
  if ddInfo > 0 then return False else
    pushMut e >> return True

ddInformation :: PrimMonad m => MutEntry -> EvolutionT m Double
ddInformation (ME mut _ ddns dnm _) = do
  m <- numSymbols
  bigN <- use stringLen
  ils <- getMutCountIntervals ddns
  nm <- use jointCount

  vm <- variety
  (sz0, sz1) <- (typeState.TS.jointType) `uses` JT.dims
  let vm' = case mut of
        AddLeft _  -> vm + sz1
        AddRight _ -> vm + sz0
        Add2 _ _   -> vm + sz0 + sz1 + 1
        DelLeft _  -> vm - sz1
        DelRight _ -> vm - sz0
        Del2 _ _   -> vm - sz0 - sz1 + 1

  return $ Math.ddInfo m bigN ils (nm, nm+dnm) (vm, vm')

-- | Return the updated count-intervals of symbols whose counts are
-- affected by a mut, given the delta-delta-symbol-counts of the mut
getMutCountIntervals :: PrimMonad m => IntMap Int -> EvolutionT m [(Count,Count)]
getMutCountIntervals ddns = do
  ns <- use symCounts
  dns <- use deltaCounts
  forM (IM.toList ddns) $ \(s,ddn) -> do
    let n = ns U.! s
    dn <- MU.read dns s
    let n' = n + dn
        n'' = n' + ddn
    seq n'' $ return (n',n'')

------------
-- UPDATE --
------------

introMut :: forall m. PrimMonad m => Mutation -> EvolutionT m ()
introMut mut = do
  tst <- use typeState
  jts <- TS.jointsOf tst mut

  allCIs <- use jointCIs
  let mutCIs = mfoldTree $ fmap (allCIs M.!) jts

  ns <- use symCounts
  dns <- use deltaCounts
  let n'Of s = ((ns U.! s)+) <$> MU.read dns s

  cor <- case typeOfMut mut of
    Add -> do
      undefined

    Del -> do
      undefined

  jt <- use $ typeState.TS.jointType -- debug
  str <- use doubly >>= D.toList -- debug
  e <- ME.fromParamsWith jt str n'Of mut mutCIs cor
  undefined


-- | Apply a mutation, update books
pushMut :: forall m. PrimMonad m => MutEntry -> EvolutionT m ()
pushMut (ME mut _ ddns dnm (CIs djt _ bhd _)) = do
  traceM $ "Pushing mutation: " ++ show mut

  -- info: correction need to be computed on the mut's CIs and their
  -- super-CIs in different order depending on if it's an add/del
  -- mutation
  let mutCIs = IM.elems bhd
      -- | Map over the CIs from the Entry.
      getMutCIsCorr :: EvolutionT m [Map Mutation (IntMap Int)]
      getMutCIsCorr = uses2 doubly typeState corrsOf
                      >>= forM mutCIs
      -- | Map over the super-CIs of the CIs of the Entry. CIs are
      -- assumed to be *inside* the type when called.
      getSuperCIsCorr :: EvolutionT m [Map Mutation (IntMap Int)]
      getSuperCIsCorr = do
        dly <- use doubly
        tst <- use typeState
        mapM (TS.superCI dly tst djt) mutCIs
          >>= mapM (maybe (return M.empty) -- mapMaybeM would unalign
                          (corrsOf dly tst))

  -- ENUMERATE CORRECTION AND APPLY MUT
  ((enabledMuts, expiredMuts), oldCorr, newCorr) <- case typeOfMut mut of
    Add -> do oldCorr <- getMutCIsCorr
              mutChange <- zoom typeState $ TS.pushMut mut -- APPLY
              (mutChange, oldCorr,) <$> getSuperCIsCorr

    Del -> do oldCorr <- getSuperCIsCorr
              mutChange <- zoom typeState $ TS.pushMut mut -- APPLY
              (mutChange, oldCorr,) <$> getMutCIsCorr -- trust

  let corrDelta = unions $ zipWith (clean .: union) newCorr $
                  negate <<<$>>> oldCorr

  -- DELETE EACH EXPIRED MUT
  zoom mutBooks $ forM_ (Set.toList expiredMuts) MB.delete
  -- INSERT NEWLY ENABLED MUTS
  forM_ (Set.toList enabledMuts) introMut

  ns <- use symCounts
  dns <- use deltaCounts

  -- UPDATE ENTRIES
  (mutBooks.byMut %=) $ flip2 (flip2 M.differenceWith) corrDelta $
    \e@(ME _ eloss eddns ednm _) cor ->
      let dloss = sum $ flip2 IM.intersectionWithKey eddns cor $
            \s eddn d -> let n = ns U.! s
                             dn = undefined -- fromMaybe 0 $ IM.lookup s dns
                             n' = n + dn
                             old_n'' = n' + eddn
                             new_n'' = old_n'' + d
                         in logFact old_n'' - logFact new_n''
          sum_cor = sum cor & \r -> if even r then r
            else err' $ "expected even number: " ++ show (r,cor)
      in Just $ e{ _ddSymCountsLoss = eloss + dloss
                 , _ddSymCounts     = IM.unionWith (+) eddns cor
                 , _dJointCount     = ednm + sum_cor }

  -- UPDATE LOSSES
  oldEntries <- use (mutBooks.byMut) -- before we modify
  readAffected <- (mutBooks.byAffected) `uses` MV.read
  dnsAffected <- fmap (fromMaybe M.empty . foldTree M.union) $
    forM (IM.toList ddns) $ \(s,ddn) -> do
    let n = ns U.! s -- count prior to intro (no change)
    old_dn <- use deltaCounts >>= flip MU.read s
    let old_n' = n + old_dn -- old count after intro
        new_n' = old_n' + ddn -- new count after intro

    affected <- readAffected s
    (mutBooks.byMut %=) $ flip2 (flip2 M.differenceWith) affected $
      \e _ ->
        let eddn = (e^.ddSymCounts) IM.! s -- entry's mut's delta (no change)
            old_n'' = old_n' + eddn -- old count after intro after mut
            oldContrib = logFact old_n' - logFact old_n''
            new_n'' = new_n' + eddn -- new count after intro after mut
            newContrib = logFact new_n' - logFact new_n''
        in Just $ e & ddSymCountsLoss %~ (+newContrib) . (+(-oldContrib))
    return affected

  -- RE-INDEXING (TODO: join corAffected to dnsAffected)
  let affected = void corrDelta `M.union` dnsAffected
  affectedNew <- (mutBooks.byMut) `uses` (`M.intersection` affected)
  let affectedOld = oldEntries `M.intersection` affected
  -- zoom mutBooks $ sequence_ $
  --   M.intersectionWith undefined ---------- FIXME: TODO -----------
  --   affectedOld affectedNew -- MutBooks.update

  deltaCounts %= undefined -- IM.unionWith (+) ddns -- delta ns
  jointCount += dnm -- delta nm

  where
    clean = M.filter (not . IM.null) . fmap (IM.filter (/= 0))
    union = M.unionWith (IM.unionWith (+))
    unions = fromMaybe M.empty . foldTree union
    err' = err . ("pushMut: " ++)

----------
-- INIT --
----------

-- | Construct a new EvolutionState
init :: PrimMonad m =>
  Int -> Int -> Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
  JointType -> m (EvolutionState (PrimState m))
init m bigN dly ns allCIs jt = init_ m bigN dly ns allCIs (jt, memJointCIs)
  where memJointCIs = M.filterWithKey (const . flip JT.member jt) allCIs

-- | Construct a new EvolutionState where the second set of CIs given is
-- a subset of the first set and corresponds exactly to its entries for
-- each joint that falls under the given type.
init_ :: forall m. PrimMonad m =>
  Int -> Int -> Doubly (PrimState m) -> U.Vector Int -> Joints CIs ->
  (JointType, Joints CIs) -> m (EvolutionState (PrimState m))
init_ m bigN dly ns allCIs (jt, memJointCIs) = do
  tst <- TS.init m allJoints jt

  cisByMut <- joinByMut tst CIs.join $ M.toList allCIs
  corByMut <- fromMaybe M.empty . foldTree union
              <$> mapM (corrsOf dly tst) (CIs.toList memCIs)

  dns_mv <- MU.replicate m (0 :: Count)
  forM_ (IM.toList dns) $ uncurry $ MU.write dns_mv

  let n'Of s = ((ns U.! s)+) <$> MU.read dns_mv s

  str <- D.toList dly -- TODO: rm
  es <- sequence $ M.mergeWithKey
        (Just .:. ME.fromParamsWith jt str n'Of) -- CIs * cor
        (M.mapWithKey $ ME.fromParams jt str n'Of) -- only CIs
        (fmap $ err' . ("have cor, but CIs missing: " ++) . show) -- only cor
        cisByMut corByMut

  books <- MB.fromList m $ M.elems es
  return $ EvolutionState { _stringLen   = bigN
                          , _doubly      = dly
                          , _symCounts   = ns
                          , _jointCIs    = allCIs
                          , _typeState   = tst
                          , _deltaCounts = dns_mv
                          , _jointCount  = nm
                          , _mutBooks       = books }
  where
    union = M.unionWith (IM.unionWith (+))
    allJoints = M.keys allCIs

    memCIs = fromMaybe CIs.empty $ foldTree CIs.join $ M.elems memJointCIs
    ndns = memCIs^.CIs.symCounts -- negative delta symbol counts

    two_nm = sum ndns
    nm | odd two_nm = err' $ "expected an even number: " ++ show (two_nm, ndns)
       | otherwise = two_nm `div` 2
    dns = negate <$> ndns -- delta symbol counts (intro's)

    err' = err . ("init: " ++)

-- WHERE --

-- | Combine values keyed by joints flipped (in/out) by the same
-- mutation together, given a combining function
joinByMut :: forall m a. PrimMonad m => TypeState (PrimState m) ->
  (a -> a -> a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMut tst f = fmap (M.fromListWith f . concat) . mapM g
  where
    g :: ((Sym,Sym), a) -> m [(Mutation, a)]
    g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

err :: String -> a
err = error . ("Evolution." ++)
