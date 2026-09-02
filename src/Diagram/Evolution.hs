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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import qualified Data.Vector.Unboxed as U

import Diagram.Pretty
import Diagram.Primitive

import Diagram.Joints (Joints)
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT
import Diagram.String
import qualified Diagram.ConstrInterval as CI
import Diagram.ConstrIntervals (CIs(CIs))
import qualified Diagram.ConstrIntervals as CIs
import qualified Diagram.Doubly as D

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..), typeOfMut)
import qualified Diagram.Evolution.Correction as Cor
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution.MutEntry (MutEntry(..))
import qualified Diagram.Evolution.MutEntry as ME
import Diagram.Evolution.MutBooks (MutBooks(MutBooks), byMut)
import qualified Diagram.Evolution.MutBooks as MB

import Diagram.Util

----------------------
-- EVOLUTION STATE  --
----------------------

type EvolutionT m = StateT (EvolutionState (PrimState m)) m
-- | Evolution state of a JointType in a given string
data EvolutionState s = EvolutionState
  -- String state (static/readonly, only changes accross intros, not evolution)
  { _stringLen :: !Int -- N, bigN
  , _doubly    :: !(Doubly s) -- dly :: underlying string :: [N]Sym
  , _symCounts :: !(U.Vector Count) -- ns :: symbol counts (TODO: dyn vec)
  , _jointCIs  :: !(Joints CIs) -- allCIs :: (s0,s1) -> CIs

  -- current Type state (evolves/mutates)
  , _typeState  :: !(TypeState s) -- sym entries :: [(mem, coIn, deps, coOut)]
  , _typeCIs    :: !CIs -- joint type CIs
  , _jointCount :: !Int -- nm (dnm)

  -- indexed Mutations
  , _mutBooks :: !(MutBooks s) }
makeLenses ''EvolutionState

-- GETTERS --

-- | m
numSymbols :: Monad m => EvolutionT m Int
numSymbols = zoom typeState TS.numSymbols

jointType :: Monad m => EvolutionT m JointType
jointType = use $ typeCIs.CIs.jointType

-- | vm = sz0 * sz1
variety :: Monad m => EvolutionT m Int
variety = JT.variety <$> jointType

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

getSymCountIntervals :: Monad m => EvolutionT m [(Sym,(Count,Count))]
getSymCountIntervals = do
  ns <- use symCounts
  CIs _ ndns _ _ <- use typeCIs
  return $ IM.toList $ flip IM.mapWithKey ndns $ \s ndn ->
    let n = ns U.! s
        n' = n - ndn
    in seq n' (n,n')

-- EVAL --

-- | Enumerate all available mutations with their loss. Unsorted.
evalAll :: Monad m => EvolutionT m [(Double, MutEntry)]
evalAll = evalAll_ <$> numSymbols -- m
                   <*> use stringLen -- N
                   <*> use jointCount -- nm
                   <*> jointType -- JointType
                   <*> use mutBooks -- Books

evalAll_ :: Int -> Int -> Int -> JointType -> MutBooks s ->
            [(Double, MutEntry)]
evalAll_ m bigN nm jt (MutBooks als ars a2s dls drs d2s _ _) =
  concat $ zipWith (fmap . toFst) lossFns entries
  where
    (sz0,sz1) = JT.dims jt
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
            >.> fmap (^.typeCIs.CIs.jointType)

step :: PrimMonad m => EvolutionT m Bool
step = do
  es <- evalAll
  let (_, e) = L.minimumBy (compare `on` fst) es
      ME mut loss ddns dnm (CIs jt _ _ _) = e

  traceM ""
  traceM $ "  Mutation: " ++ pShow mut
  traceM $ "  ddnsLoss: " ++ pShow loss
  traceM $ "  ddns: "     ++ pShow ddns
  traceM $ "  dnm: "      ++ pShow dnm

  ddInfo <- ddInformation e
  traceM $ "  ddInfo: "      ++ pShow ddInfo
  traceM ""

  str <- D.toList =<< use doubly -- (debug)
  CIs typJT _ _ _ <- use typeCIs
  let typJT' = JT.appMut mut typJT
  traceM $ "Constructions before:\n" ++ pShowStr typJT str ++ "\n\n"
    ++ "Delta:\n" ++ pShowStr jt str ++ "\n\n"
    ++ "Constructions after:\n" ++ pShowStr typJT' str ++ "\n"

  if ddInfo > 0
    then do
    return False

    else do
    traceM $ "Pushing mut " ++ pShow mut ++ "\n"
    pushMut e
    traceM "----------------------------------------------------------"
    return True

ddInformation :: PrimMonad m => MutEntry -> EvolutionT m Double
ddInformation (ME mut _ ddns dnm _) = do
  m <- numSymbols
  bigN <- use stringLen
  ils <- getMutCountIntervals ddns
  nm <- use jointCount

  vm <- variety
  (sz0, sz1) <- JT.dims <$> jointType
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
  CIs _ ndns _ _ <- use typeCIs
  return $ IM.elems $ IM.mergeWithKey (Just .:. f ns)
    (const IM.empty) (IM.mapWithKey $ flip (f ns) 0)
    ndns ddns
  where
    f ns s ndn ddn = seq n'' (n',n'')
      where n = ns U.! s
            n' = n - ndn
            n'' = n' + ddn

------------
-- UPDATE --
------------

-- | Apply a mutation, update books
pushMut :: forall m. PrimMonad m => MutEntry -> EvolutionT m ()
pushMut (ME mut _ mutDdns mutDnm mutCIs@(CIs mutJT _ mutCIsBhd _)) = do
  CIs _ typNdns _ _ <- use typeCIs -- before we update it

  let mutCIsL = IM.elems mutCIsBhd
  -- ENUMERATE CORRECTION AND APPLY MUT (IN THE RIGHT ORDER)
  (enabledMuts, expiredMuts, mutCorDelta) <- case typeOfMut mut of
    Add -> do
      -- APPLY BEFORE PASS
      (enabled, expired) <- zoom typeState $ TS.pushMut mut -- APPLY
      -- CORRECTIONS AFTER (FOR mutCIs TO BE <: typCIs)
      getSuperCI <- uses2 doubly typeState TS.superCI ?? mutJT
      let sub = not . (`JT.member` mutJT)
      getCorrsOf <- uses2 doubly typeState $ Cor.onAllMuts sub
      corrsDelta <- flip execStateT M.empty $ forM mutCIsL $
        \ci -> (getSuperCI (traceShowId ci) >>=) $ flip whenJust $ \case
          Nothing -> modify . union =<< getCorrsOf ci -- no adjacents
          Just (super, adjacent) -> do
            traceShowM ("super", super)
            traceShowM ("adjacents", adjacent)
            adjCorrs <- sequence $ getCorrsOf <$> adjacent
            traceShowM ("adjCorrs", adjCorrs)
            superCorr <- getCorrsOf super
            traceShowM ("superCorr", superCorr)
            let delta = -- note: will include corrs on enabled muts too
                  L.foldl' union superCorr $ negate <<<$>>> adjCorrs
            traceShowM ("delta", delta)
            modify (union delta)

      -- UPDATE TYPE CIs (join)
      typeCIs %= CIs.join mutCIs
      -- TODO: couldn't CIs.join give us adjacent for free?

      return ( enabled, expired
             , corrsDelta `M.withoutKeys` enabled )

    Del -> do
      -- CORRECTIONS BEFORE (WHILE mutCIs <: typCIs)
      dly <- use doubly
      getSuperCI <- uses2 doubly typeState TS.superCI ?? mutJT
      let sub = const False -- TODO: verify this
      getCorrsOf <- uses2 doubly typeState $ Cor.onAllMuts sub
      corrsDelta <- flip execStateT M.empty $ forM_ mutCIsL $
        \ci -> (getSuperCI (traceShowId ci) >>=) $ flip whenJust $ \case
          Nothing -> modify . union . ffmap negate =<< getCorrsOf ci -- no rem
          Just (super, rems) -> do
            traceShowM ("super", super) --
            traceShowM ("rems", rems) --
            -- UPDATE TYPE CIs (delete super, insert remainder)
            lift $ typeCIs %== ( L.foldl' (>=>) (CIs.deleteExisting dly super) $
                                 CIs.insertDisjoint dly <$> rems )
            old <- getCorrsOf super
            traceShowM ("old", old) --
            new <- sequence $ getCorrsOf <$> rems
            traceShowM ("new", new) --
            let delta = -- note: will include corrs on expired muts too
                  L.foldl' union (negate <<$>> old) new
            traceShowM ("delta", delta) --
            modify (union delta)

      -- APPLY AFTER PASS
      (enabled, expired) <- zoom typeState $ TS.pushMut mut -- APPLY
      -- UPDATE TYPE CIs JOINT TYPE
      typeCIs.CIs.jointType %= case mut of
        DelLeft s0  -> JT.deleteLeftMember s0
        DelRight s1 -> JT.deleteRightMember s1
        Del2 s0 s1  -> JT.deleteLeftMember s0 . JT.deleteRightMember s1
        _else -> error "impossible"

      return ( enabled, expired
             , corrsDelta `M.withoutKeys` expired )

  -- DELETE EACH EXPIRED MUT
  zoom mutBooks $ mapM_ MB.delete $ Set.toList expiredMuts
  -- INSERT EACH NEWLY ENABLED MUTS
  mapM_ introMut $ Set.toList enabledMuts
  -- TODO: fish out recip of mut from enabledMuts and intro it directly
  -- without going through introMut?

  -- UPDATE MUT BOOKS
  ns <- use symCounts
  let countUpdateIntervals = IM.mergeWithKey
        ( \s ndn ddn ->
           let n = ns U.! s
               old_n' = n - ndn
               new_n' = old_n' + ddn
               dLoss = logFact new_n' - logFact old_n'
           in seq dLoss $ Just (old_n', new_n', dLoss) )
        ( const IM.empty ) -- ndns only
        ( IM.mapWithKey $ \s ddn ->
            let n = ns U.! s
                old_n' = n -- dn == 0 by abstentia
                new_n' = old_n' + ddn
                dLoss = logFact new_n' - logFact old_n'
            in seq dLoss (old_n', new_n', dLoss) )
        typNdns mutDdns

  getAffectedMuts <- mutBooks `uses` MB.affectedMuts
  let unionIl = M.unionWithKey $
        const $ IM.unionWithKey
        (err' . ("duplicate sym count intervals: " ++) . show .:. (,,))
  countUpdateIlsByAffected <- fmap (fromMaybe M.empty . foldTree unionIl) $
                              forM (IM.toList countUpdateIntervals) $
                              \(s,ddn) -> M.fromSet (const $ IM.singleton s ddn)
                                          <$> getAffectedMuts s

  -- mutEntryUpdate :: COUNT_UPDATE * CORR_UPDATE
  let mutEntryUpdates = M.mergeWithKey (\_ -> Just .: (,))
                        ((,IM.empty) <$>) ((IM.empty,) <$>)
                        countUpdateIlsByAffected mutCorDelta

  -- (debug)
  CIs jt ndns _ _ <- use typeCIs
  str <- use doubly >>= D.toList
  let n'Of s = maybe n (n-) $ IM.lookup s ndns
        where n = ns U.! s
  --

  mutEntries <- use $ mutBooks.byMut
  sequence_ $ flip2 M.intersectionWith
    mutEntries mutEntryUpdates $
    \e@(ME eMut eDnsLoss eDdns eDnm _) (nsIls, deltaCor) -> do
      let signedDeltaCor = case typeOfMut eMut of
            Add -> negate <$> deltaCor
            Del -> deltaCor
          eDdnsIls = -- zip eDdns eDdns'
            IM.mergeWithKey (\_ ddn c -> Just (ddn, ddn + c))
            (const IM.empty) ((0,) <$>) eDdns signedDeltaCor
          deDnsLoss = sum $ IM.mergeWithKey
            ( \_ (old_n', new_n', dLoss) (eDdn, eDdn') -> Just $
              let old_n'' = old_n' + eDdn
                  -- old_loss = logFact old_n' - logFact old_n''
                  new_n'' = new_n' + eDdn'
                  -- new_loss = logFact new_n' - logFact new_n''
              in dLoss - logFact new_n'' + logFact old_n'' )
            ( const IM.empty ) -- no eDdn, no cor ==> no dnsLoss
            ( IM.mapWithKey $ \s (eDdn, eDdn') -> -- cor only
                let n       = ns U.! s
                    ndn     = fromMaybe 0 $ IM.lookup s typNdns
                    n'      = n - ndn -- old == new
                    old_n'' = n' + eDdn
                    new_n'' = n' + eDdn'
                in logFact old_n'' - logFact new_n'' )
                nsIls eDdnsIls
          deDnm = negate (sum signedDeltaCor) & \r ->
            if even r then r `div` 2
            else err' $ "expected even number: " ++ show (r,signedDeltaCor)
      zoom mutBooks $ -- update state
        MB.update $
        ME.validate jt str n'Of $ -- (debug)
        e{ _ddSymCountsLoss = eDnsLoss + deDnsLoss
         , _ddSymCounts     = IM.union (snd <$> eDdnsIls) eDdns
         , _dJointCount     = eDnm + deDnm }

  jointCount += mutDnm -- delta nm

  where
    union = M.unionWith (IM.unionWith (+))
    err' = err . ("pushMut: " ++)

-- WHERE --

introMut :: forall m. PrimMonad m => Mutation -> EvolutionT m ()
introMut mut = do
  tst <- use typeState
  jts <- TS.jointsOf tst mut
  allCIs <- use jointCIs
  let mutCIs@(CIs mutJT _ bhd _) = mfoldTree $ fmap (allCIs M.!) jts
      mutCIsL = IM.elems bhd

  typCIs@(CIs jt ndns _ _) <- use typeCIs
  cor <- fmap clean $ case typeOfMut mut of
    Add -> return $ snd $ CIs.join_ typCIs mutCIs
    Del -> do
      dly <- use doubly
      flip execStateT IM.empty $ forM_ mutCIsL $ \ci ->
        (lift (TS.superCI dly tst mutJT ci) >>=) $ \case
        Just Nothing -> return () -- super is identical, do nothing
        Nothing -> do -- super doesn't start here, but ci is inside it
          ciCounts <- lift (CI.symCounts dly ci)
          modify (IM.unionWith (+) (negate <$> ciCounts))
        Just (Just (super, remainder)) -> do -- subtract subs from super
          subCounts <- lift $ mapM (CI.symCounts dly) (ci:remainder)
          supCounts <- lift (CI.symCounts dly super)
          let delta = IM.unionWith (+) supCounts $ negate <$> unions subCounts
          modify (IM.unionWith (+) delta)

  str <- D.toList =<< use doubly -- (debug)
  ns <- use symCounts
  zoom mutBooks $ MB.insert $
    ME.validate jt str (n'Of ns ndns) $ -- (debug)
    ME.fromParamsWith (n'Of ns ndns) mut mutCIs cor

  where
    clean = IM.filter (/= 0)
    unions = fromMaybe IM.empty . foldTree (IM.unionWith (+))
    n'Of ns ndns s = maybe n (n-) $ IM.lookup s ndns
      where n = ns U.! s

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

  -- TODO: switch back to non-debug CIs.join --
  cisByMut <- joinByMutM tst (CIs.debug_join dly) $ M.toList allCIs
  let sub = const False -- always cancel if another in-CI immediately prec.
  corByMut <- unions <$> mapM (Cor.onAllMuts sub dly tst) memCIsL
  str <- D.toList dly -- TODO: rm
  let es = M.mergeWithKey
        (Just . ME.validate jt str n'Of .:. ME.fromParamsWith n'Of) -- CIs * cor
        (M.mapWithKey $
          ME.validate jt str n'Of .: ME.fromParams n'Of) -- only CIs
        (fmap $ err' . ("have cor, but CIs missing: " ++) . show) -- only cor
        cisByMut corByMut

  books <- MB.fromList m $ M.elems es
  return $ EvolutionState { _stringLen  = bigN
                          , _doubly     = dly
                          , _symCounts  = ns
                          , _jointCIs   = allCIs
                          , _typeState  = tst
                          , _typeCIs    = memCIs
                          , _jointCount = nm
                          , _mutBooks   = books }
  where
    union = M.unionWith (IM.unionWith (+))
    unions = fromMaybe M.empty . foldTree union
    allJoints = M.keys allCIs
    memCIs@(CIs _ ndns _ _) = mfoldTree $ M.elems memJointCIs
    memCIsL = CIs.toList memCIs
    n'Of s = maybe n (n-) $ IM.lookup s ndns
      where n = ns U.! s
    err' = err . ("init: " ++)

    two_nm = sum ndns
    nm | even two_nm = two_nm `div` 2
       | otherwise = err' $ "expected an even number: " ++ show (two_nm, ndns)

-- WHERE --

-- | Combine values keyed by joints flipped (in/out) by the same
-- mutation together, given a combining function
joinByMut :: forall m a. PrimMonad m => TypeState (PrimState m) ->
  (a -> a -> a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMut tst f = fmap (M.fromListWith f . concat) . mapM g
  where
    g :: ((Sym,Sym), a) -> m [(Mutation, a)]
    g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

-- | joinByMut, but monadic
joinByMutM :: forall m a. PrimMonad m => TypeState (PrimState m) ->
  (a -> a -> m a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMutM tst f = (fromListWithM f . concat) <=< mapM g
  where
    g :: ((Sym,Sym), a) -> m [(Mutation, a)]
    g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

-- | Data.Map.fromListWith, but monadic
fromListWithM :: (Ord k, Monad m) => (a -> a -> m a) -> [(k, a)] -> m (Map k a)
fromListWithM f = foldM g M.empty
  where
    g m (k,v) = M.alterF (comb v) k m
    comb new Nothing    = pure (Just new)
    comb new (Just old) = Just <$> f new old

err :: String -> a
err = error . ("Evolution." ++)
