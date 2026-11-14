{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Evolution.Run (module Diagram.Evolution.Run) where

import Prelude hiding (init)

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>),index)
import Control.Monad.State.Strict

import Data.Maybe
import qualified Data.List as L
import Data.Strict.Tuple (Pair(..),(:!:))
import Data.Strict.Classes (toLazy)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntSet as IS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Streaming
import qualified Streaming.Prelude as S

import Diagram.Pretty
import Diagram.Progress
import Diagram.Primitive

import Diagram.Joints (Joints)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import Diagram.String
import qualified Diagram.ConstrInterval as CI
import Diagram.ConstrInterval (CI)
import Diagram.ConstrIntervals (CIs(CIs))
import qualified Diagram.ConstrIntervals as CIs
import qualified Diagram.Doubly as D

import Diagram.Evolution.Math (logFact)
import qualified Diagram.Evolution.Math as Math
import Diagram.Evolution.Mutation (Mutation(..), MutType(..))
import qualified Diagram.Evolution.Mutation as Mut
import qualified Diagram.Evolution.Correction as Cor
import Diagram.Evolution.TypeState (TypeState)
import qualified Diagram.Evolution.TypeState as TS
import Diagram.Evolution.MutEntry (MutEntry(..))
import qualified Diagram.Evolution.MutEntry as ME
import Diagram.Evolution.MutEntry.Update (Update(..))
import qualified Diagram.Evolution.MutEntry.Update as MEU
import Diagram.Evolution.MutBooks (MutBooks(MutBooks), byMut)
import qualified Diagram.Evolution.MutBooks as MB

import Diagram.Util

----------------
-- RUN STATE  --
----------------

type RunT m = StateT (RunState (PrimState m)) m
-- | Run state of a JointType in a given string
data RunState s = RunState
  -- string state (static)
  { _stringLen :: !Int -- N, bigN
  , _doubly    :: !(Doubly s) -- dly :: underlying string :: [N]Sym
  , _symCounts :: !(U.Vector Count) -- ns :: symbol counts (TODO: dyn vec?)
  , _jointCIs  :: !(Joints CIs) -- allCIs :: (s0,s1) -> CIs

  , _runNumber  :: !Int -- number of times (re-)initialized
  -- type state attributes (evolve/mutate)
  , _stepNumber :: !Int -- number of steps taken (mutations applied)
  , _typeState  :: !(TypeState s) -- sym entries :: [(mem, coIn, deps, coOut)]
  , _typeCIs    :: !CIs -- joint type CIs
  , _jointCount :: !Int -- nm (dnm)

  -- indexed mutations
  , _mutBooks :: !(MutBooks s) }
makeLenses ''RunState

-- GETTERS --

-- | m
numSymbols :: Monad m => RunT m Int
numSymbols = zoom typeState TS.numSymbols

jointType :: Monad m => RunT m JointType
jointType = use $ typeCIs.CIs.jointType

-- | vm = sz0 * sz1
variety :: Monad m => RunT m Int
variety = JT.variety <$> jointType

-- | Compute the difference in information/code length incurred by the
-- introduction of the current joint type (i.e. no further mutation)
getIntroInfo :: PrimMonad m => RunT m Double
getIntroInfo = Math.dInfo <$> numSymbols -- m
                          <*> use stringLen -- N
                          <*> use jointCount -- nm
                          <*> getCountIntervals -- [(n,n')]
                          <*> variety -- vm

getCountIntervals :: PrimMonad m => RunT m [(Count,Count)]
getCountIntervals = snd <<$>> getSymCountIntervals

getSymCountIntervals :: Monad m => RunT m [(Sym,(Count,Count))]
getSymCountIntervals = do
  ns <- use symCounts
  CIs _ ndns _ _ <- use typeCIs
  return $ IM.toList $ flip IM.mapWithKey ndns $ \s ndn ->
    let n = ns U.! s
        n' = n - ndn
    in seq n' (n,n')

-- EVAL --

-- | Enumerate all available mutations with their loss. Unsorted.
streamLosses :: MonadIO m => Stream (Of (Double, MutEntry)) (RunT m) ()
streamLosses = join $ streamLosses_ <$> lift numSymbols -- m
                                    <*> lift (use stringLen) -- N
                                    <*> lift (use jointCount) -- nm
                                    <*> lift jointType -- JointType
                                    <*> lift (use mutBooks) -- Books

streamLosses_ :: forall m s. MonadIO m => Int -> Int -> Int -> JointType ->
                 MutBooks s -> Stream (Of (Double, MutEntry)) m ()
streamLosses_ m bigN nm jt (MutBooks als ars a2s dls drs d2s allMuts _) = do
  withPB booksSize "Evaluating losses" $
    sequence_ $ zipWith (S.map . toFst) lossFns entries
  where
    booksSize = M.size allMuts
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

    -- TODO: optimize only walking through necessary start of maps
    entries :: [Stream (Of MutEntry) m ()]
    entries = S.each . flatten <$> [ als, ars, a2s, dls, drs, d2s ]
    flatten = concatMap (concatMap M.elems . M.elems) . IM.elems

-- EXEC --

step :: (PrimMonad m, MonadIO m) => RunT m Bool
step = do
  m <- numSymbols
  CIs (JT u0 u1) _ _ _ <- use typeCIs
  liftIO $ putStrLn $ "\nJoint type: ( 0x" ++ UT.toHex m u0
  liftIO $ putStrLn $   "            , 0x" ++ UT.toHex m u1 ++ " )\n"

  e@(ME mut loss _ddns dnm _) <- (S.next streamLosses >>=) $ \case
    Left () -> err "step: empty books"
    Right (lme0,mes) -> flip3 S.fold_ lme0 snd mes $ \lme lme' ->
      if fst lme' < fst lme then lme' else lme
  liftIO $ putStrLn $ "\n  Best mutation: " ++ pShow mut
  liftIO $ putStrLn $ "  ddnsLoss: " ++ pShow loss
  -- liftIO $ putStrLn $ "  ddns: "     ++ show ddns
  liftIO $ putStrLn $ "  dnm: "      ++ pShow dnm
  ddInfo <- ddInformation e
  liftIO $ putStrLn $ "  ddInfo: "      ++ pShow ddInfo ++ " nats\n"

  -- str <- D.toList =<< use doubly -- (debug)
  -- liftIO $ putStrLn $ pShowStrMut mut mutJT typJT str

  if ddInfo > 0 then return False else do
    stepNumber += 1 -- inc
    -- numRun <- use runNumber
    -- numStep <- use stepNumber
    -- let msg = "-- Pushing mut " ++ show mut ++ " --"
    --     msg_len = length msg
    --     bot_bar = replicate msg_len '-'
    --     top_bar_label = '[' : show numRun ++ ':' : show numStep ++ [']']
    --     label_len = length top_bar_label
    --     top_bar_left_side_len = (msg_len - label_len) `div` 2
    --     top_bar_left_side = replicate top_bar_left_side_len '-'
    --     top_bar_right_side = replicate (msg_len - label_len
    --                                     - top_bar_left_side_len) '-'
    -- liftIO $ putStrLn $ ' ':(top_bar_left_side ++ top_bar_label ++ top_bar_right_side)
    -- liftIO $ putStrLn $ ' ':msg
    -- liftIO $ putStrLn $ ' ':bot_bar
    -- liftIO $ putStrLn ""
    pushMut e
    -- trace "" liftIO $ putStrLn $ " -------------------- Finished push "
    --   ++ top_bar_label ++ " --------------------"
    return True

ddInformation :: PrimMonad m => MutEntry -> RunT m Double
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
getMutCountIntervals :: PrimMonad m => IntMap Int -> RunT m [(Count,Count)]
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

-- | Apply a mutation, update books.
pushMut :: forall m. (PrimMonad m, MonadIO m) => MutEntry -> RunT m ()
pushMut me@(ME mut _ mutDdns mutDnm _) = do

  -- Application of a Mutation affects other MutEntries in three ways:
  -- (1) the post-intro count of symbols (n') changes, affecting mut
  -- entries' post-application symbol counts (n''), both of which
  -- dnsLoss is a function of; (2) changes to correction terms from the
  -- inclusion/exclusion of the muts CIs, affecting mut entries'
  -- post-application symbol counts (n''); and (3) inclusion/exclusion
  -- of new joints into mut CIs, again affecting count n'' and the
  -- dnsLoss. Diagram.Evolution.MutEntry.Update (MEU) makes the three
  -- into update "entries" to be applied together for each affected
  -- mutation.

  old_tst <- TS.clone =<< use typeState -- clone entire type state
  zoom typeState $ TS.pushMut mut -- [APPLY] on TypeState
  new_tst <- use typeState
  --

  -- (3/3): Joint coverage changes
  (enabledMuts, expiredMuts, movedJoints) <- TS.deltaMutJoints old_tst mut
  -- traceM $ "Enabled muts (insert): " ++ pShow enabledMuts ++ "\n"
  -- traceM $ "Expired muts (delete): " ++ pShow expiredMuts ++ "\n"
  -- traceM $ "Moved (" ++ show typeOfMut ++ "'d) joints: " ++ pShow movedJoints ++ "\n"

  -- DELETE EXPIRED ENTRIES
  zoom mutBooks $ mapM_ MB.delete $ Set.toList expiredMuts
  --

  -- (1/3): Pre-mut symbol count changes (n')
  CIs _ oldTypNdns _ _ <- use typeCIs -- (before getCorDelta modifies)
  ns <- use symCounts
  let -- movedMuts = enabledMuts `Set.union` expiredMuts
      -- (moved muts no longer returned by Cor.deltaOnAllMuts)
    n'Ils = pushMut_n'Ils ns oldTypNdns mutDdns
    unionIl = M.unionWithKey $ const $ IM.unionWithKey
              (err' . ("duplicate sym count intervals: " ++) . show .:. (,,))
  getAffectedMuts <- mutBooks `uses` MB.affectedMuts

  n'IlsByAffected <- fmap ( fromMaybe M.empty
                          -- maybe M.empty (flip M.withoutKeys movedMuts)
                           . foldTree unionIl ) $
    forM (IM.toList n'Ils) $ \(s, n'Il) ->
    M.fromSet (const $ IM.singleton s n'Il) <$> getAffectedMuts s
  --

  -- (2/3): Correction terms changes
  let movedJointss :: Map Mutation (Set (Sym, Sym))
      movedJointss = flip M.mapWithKey movedJoints $ \mu ss ->
        ( Set.fromDistinctAscList $ Mut.mutJoint mu <$> IS.toAscList ss )

  dly <- use doubly
  dCor <- typeCIs %%== pushMutCIs dly old_tst new_tst me
          -- <&> (`M.withoutKeys` movedMuts)
  --

  -- MAKE ENTRY UPDATES
  let mkMEU_01 nIl dc = MEU nIl dc CIs.empty
      mutEntryUpdates_01 = M.mergeWithKey (const $ Just .: mkMEU_01)
                           (MEU.fromDCounts <$>) (MEU.fromDCor <$>)
                           n'IlsByAffected dCor
  allCIs <- use jointCIs
  let getCIs = mfoldTree . M.elems . M.restrictKeys allCIs

      mutEntryUpdates = M.mergeWithKey
        (\_ (MEU nIl dcor _) dcis -> Just $ MEU nIl dcor dcis)
        id (fmap MEU.fromDCIs)
        mutEntryUpdates_01 (fmap getCIs movedJointss)

  -- (debug)
  CIs _newTypJT ndns _ _ <- use typeCIs
  -- str <- use doubly >>= D.toList
  let n'Of s = maybe n (n-) $ IM.lookup s ndns
        where n = ns U.! s
  --

  -- APPLY ENTRY UPDATES
  mutEntries <- use $ mutBooks.byMut
  let mutEntryUpdatesCount = M.size mutEntryUpdates
  pb <- liftIO $ newPB mutEntryUpdatesCount "Updating affected mutation entries"
  let apply :: MutEntry -> Update -> StateT (MutBooks (PrimState m)) m MutEntry
      apply = MEU.apply old_tst typeOfMut new_tst n'Of dly
      update :: MutEntry -> Update -> StateT (MutBooks (PrimState m)) m ()
      update = apply >==> MB.update >=> const (liftIO $ incPB pb)
      -- . ME.validate newTypJT str n'Of

  zoom mutBooks $ sequence_ $ M.intersectionWith update
    mutEntries mutEntryUpdates

  -- INSERT NEWLY ENABLED ENTRIES
  let ime@(ME imut _ _ _ _) = ME.recip me
  es <- mapM mkMutEntry $ Set.toList $ Set.delete imut enabledMuts
  zoom mutBooks $ mapM_ MB.insert (ime:es)
  --

  jointCount += mutDnm -- apply delta nm
  -- get >>= validate -- (debug)

  where
    typeOfMut = Mut.typeOfMut mut
    err' = err . ("pushMut: " ++)

-- WHERE --

-- | Given the string, before and after type states of the given mut as
-- well as the CIs of the type before application of the mut, apply the
-- difference on the type CIs and the delta on Cor of all available
-- mutations (may include erroneous corrections on newly enabled muts
-- (Add case) or newly expired muts (Del case), which need to be
-- computed explicitly anyway, but remember to filter them out)
pushMutCIs :: forall m. (PrimMonad m, MonadIO m) => Doubly (PrimState m) ->
  TypeState (PrimState m) -> TypeState (PrimState m) -> MutEntry -> CIs ->
  m (Map Mutation (IntMap Int), CIs)
pushMutCIs dly old_tst new_tst me typCIs =
  fmap toLazy $ S.effects $ flip execStateT st0 $
  distribute $ S.mapM procMutCI $
  withPB mutCIsCount "Applying mutation to type CIs" $
  S.each $ IM.elems mutCIsBhd
  where
    ME mut _ _ _ (CIs _ _ mutCIsBhd _) = me
    (procMutCI, sub_tst, sup_tst) = case Mut.typeOfMut mut of
      Add -> (procAddMutCI, old_tst, new_tst)
      Del -> (procDelMutCI, new_tst, old_tst)

    superCI :: CI -> m (Maybe (Maybe (CI, [CI])))
    superCI = CI.superCI dly memSup memMut
      where memSup = TS.member sup_tst
            memMut = fmap not .: TS.member sub_tst

    getDelta :: CI -> [CI] -> m (Map Mutation (IntMap Int))
    getDelta super subs = Cor.deltaOnAllMuts dly
                          (super, sup_tst) (subs, sub_tst)

    procAddMutCI :: CI -> StateT (Map Mutation (IntMap Int) :!: CIs) m ()
    procAddMutCI mutCI = (lift (superCI mutCI) >>=) $ flip whenJust $ \case
      Nothing -> do -- super == ci
        (_1 %=) . Cor.union =<< lift (getDelta mutCI [])
        _2 %== insert1 mutCI
      Just (super, subs) -> do -- super > ci
        (_1 %=) . Cor.union =<< lift (getDelta super subs)
        _2 %== foldr (>=>) (insert1 super) -- insert after
                           (delete1 <$> subs) -- dels first

    procDelMutCI :: CI -> StateT (Map Mutation (IntMap Int) :!: CIs) m ()
    procDelMutCI mutCI = (lift (superCI mutCI) >>=) $ flip whenJust $ \case
      Nothing -> do -- super == ci
        (_1 %=) . Cor.union =<< lift (getDelta mutCI [])
        _2 %== delete1 mutCI
      Just (super, subs) -> do -- super > ci
        (_1 %=) . Cor.union =<< lift (getDelta super subs)
        _2 %== foldr (<=<) (delete1 super) -- del first
                           (insert1 <$> subs) -- inserts after

    -- we modify the JT here, and do all the insert-deleting in procs
    st0 = M.empty :!: cis0
    cis0 = over CIs.jointType (Mut.unsafeApply mut) typCIs

    mutCIsCount = IM.size mutCIsBhd -- TODO: bookkeep?
    insert1 = CIs.insertDisjoint dly
    delete1 = CIs.deleteExisting dly

-- | Given a count vector (before intro, `n`), counts of the symbols in
-- the type to be introduced (negative delta on counts, `-dn`), and
-- deltas of sym counts on that type of a mut (delta delta, `ddn`),
-- return the before/after values of delta on counts (n,n') as well as
-- the delta on the dnsLoss that is incured by the change.
pushMut_n'Ils :: U.Vector Count -> IntMap Int -> IntMap Int ->
                  IntMap (Count, Count, Double)
pushMut_n'Ils ns = IM.mergeWithKey col
                   (const IM.empty) -- ndns only
                   (IM.mapWithKey newDns)
  where
    col :: Int -> Count -> Count -> Maybe (Count, Count, Double)
    col s ndn ddn = seq dLoss $ Just (old_n', new_n', dLoss)
      where n = ns U.! s
            old_n' = n - ndn
            new_n' = old_n' + ddn
            dLoss = logFact new_n' - logFact old_n'

    newDns :: Int -> Count -> (Count, Count, Double)
    newDns s ddn = seq dLoss (old_n', new_n', dLoss)
      where n = ns U.! s
            old_n' = n -- dn == 0
            new_n' = old_n' + ddn
            dLoss = logFact new_n' - logFact old_n'

mkMutEntry :: PrimMonad m => Mutation -> RunT m MutEntry
mkMutEntry mut = do
  -- traceM $ "Intro'ing mut: " ++ pShow mut
  tst <- use typeState
  allCIs <- use jointCIs
  mutCIs <- mfoldTree . M.elems . M.intersection allCIs
            <$> TS.jointsOfMut tst mut

  typCIs@(CIs _jt ndns _ _) <- use typeCIs
  -- str <- D.toList =<< use doubly -- (debug)
  ns <- use symCounts
  cor <- case Mut.typeOfMut mut of
    Add -> return $ snd $ CIs.join_ typCIs mutCIs
    Del -> do
      dly <- use doubly
      fmap snd $ CIs.difference dly (Just $ TS.member tst)
         Nothing typCIs mutCIs

  return $ -- ME.validate jt str (n'Of ns ndns) $ -- (debug)
    ME.fromParamsWithCor (n'Of ns ndns) mut mutCIs cor

  where
    n'Of ns ndns s = maybe n (n-) $ IM.lookup s ndns
      where n = ns U.! s

----------
-- INIT --
----------

-- | Initialize a new RunState
init :: (PrimMonad m, MonadIO m) => Int -> Int -> Doubly (PrimState m) ->
  U.Vector Int -> Joints CIs -> Int -> JointType -> m (RunState (PrimState m))
init m bigN dly ns allCIs num jt = init_ m bigN dly ns
                                   allCIs num (jt, memJointCIs)
  where
    memJointCIs = M.filterWithKey (const . uc (JT.member jt)) allCIs

-- | Construct a new RunState where the second set of CIs given is a
-- subset of the first set and corresponds exactly to its entries for
-- each joint that falls under the given type.
init_ :: forall m. (PrimMonad m, MonadIO m) => Int -> Int ->
         Doubly (PrimState m) -> U.Vector Int -> Joints CIs -> Int ->
         (JointType, Joints CIs) -> m (RunState (PrimState m))
init_ m bigN dly ns allCIs num (jt, memJointCIs) = do
  tst <- TS.init m allJoints jt

  let jointsCount = M.size allCIs
      insertByMut = flip $ uc $ uc $ insertByMutWith tst CIs.join
  cisByMut <- S.foldM_ insertByMut (return M.empty) return $
              withPB jointsCount "Merging CIs by mutation" $
              S.each $ M.toList allCIs

  let memCIsCount = IM.size $ memCIs^.CIs.byHead -- TODO: bookkeep size?
      insertCor ci = (Cor.onAllMuts dly tst ci <&>) . Cor.union
  corByMut <- S.foldM_ (flip insertCor) (return M.empty) return $
              withPB memCIsCount "Counting all correction terms" $
              S.each $ CIs.toList memCIs

  let es = M.mergeWithKey ( Just -- . ME.validate jt str n'Of
                            .:. ME.fromParamsWithCor n'Of ) -- CIs * cor
        (M.mapWithKey $ -- ME.validate jt str n'Of .:
          ME.fromParams n'Of) -- only CIs
        (M.mapWithKey $ -- note: mergeWithKey can pass empty maps, so map errs
          err' . ("have cor, but CIs missing: " ++) . show .: (,)) -- only cor
        cisByMut corByMut

  books <- MB.fromList m $ M.elems es
  return $ RunState { _stringLen  = bigN
                    , _doubly     = dly
                    , _symCounts  = ns
                    , _jointCIs   = allCIs
                    , _runNumber  = num
                    , _stepNumber = 0
                    , _typeState  = tst
                    , _typeCIs    = memCIs
                    , _jointCount = nm
                    , _mutBooks   = books }
  where
    allJoints = M.keys allCIs
    memCIs@(CIs _ ndns _ _) = mfoldTree $ M.elems memJointCIs
    n'Of s = maybe n (n-) $ IM.lookup s ndns
      where n = ns U.! s
    err' = err . ("init: " ++)

    two_nm = sum ndns
    nm | even two_nm = two_nm `div` 2
       | otherwise = err' $ "expected an even number: " ++ show (two_nm, ndns)

-- WHERE --

-- | Insert at every mutation of the given joint, given a combining
-- function
insertByMutWith :: forall m a. PrimMonad m => TypeState (PrimState m) ->
  (a -> a -> a) -> Sym -> Sym -> a -> Map Mutation a -> m (Map Mutation a)
insertByMutWith tst f s0 s1 a = (TS.mutsOf tst s0 s1 <&>) . L.foldl' g
  where g :: Map Mutation a -> Mutation -> Map Mutation a
        g m mu = M.insertWith f mu a m

-- | Combine values keyed by joints flipped (in/out) by the same
-- mutation together, given a combining function
joinByMutWith :: forall m a. PrimMonad m => TypeState (PrimState m) ->
                 (a -> a -> a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMutWith tst f = fmap (M.fromListWith f . concat) . mapM g
  where g :: ((Sym,Sym), a) -> m [(Mutation, a)]
        g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

-- | joinByMutWith, but monadic, was used for CIs.debug_join
joinByMutWithM :: forall m a. PrimMonad m => TypeState (PrimState m) ->
                  (a -> a -> m a) -> [((Sym,Sym), a)] -> m (Map Mutation a)
joinByMutWithM tst f = (fromListWithM f . concat) <=< mapM g
  where g :: ((Sym,Sym), a) -> m [(Mutation, a)]
        g ((s0,s1), a) = (,a) <<$>> TS.mutsOf tst s0 s1

-- | Data.Map.fromListWith, but monadic
fromListWithM :: (Ord k, Monad m) => (a -> a -> m a) -> [(k, a)] -> m (Map k a)
fromListWithM f = foldM g M.empty
  where g m (k,v) = M.alterF (comb v) k m
        comb new Nothing    = pure (Just new)
        comb new (Just old) = Just <$> f new old

-----------
-- DEBUG --
-----------

validate :: (PrimMonad m, MonadIO m) => RunState (PrimState m) -> m ()
validate (RunState bigN dly ns allCIs _ _ tst memCIs nm books) = do
  st' <- init m bigN dly ns allCIs 0 jt
  let RunState _ _ _ _ _ _ tst' memCIs' nm' books' = st'

  unless (nm == nm') $
    err'' "joint counts (total)" (nm, nm')
  return $ validateCIs "Run.validate: mismatch in type CIs: "
    memCIs memCIs'

  let TS.TS u0 u1 = tst
  se0s <- V.toList <$> V.freeze u0
  se1s <- V.toList <$> V.freeze u1
  let TS.TS u0' u1' = tst'
  se0s' <- V.toList <$> V.freeze u0'
  se1s' <- V.toList <$> V.freeze u1'
  forM_ (zip [0::Int ..] $ zip se0s se0s') $ \(s, (se0, se0')) ->
    unless (se0 == se0') $
    err'' ("sym entries for " ++ show s ++ " (left)") (se0,se0')
  forM_ (zip [0::Int ..] $ zip se1s se1s') $ \(s, (se1, se1')) ->
    unless (se1 == se1') $
    err'' ("sym entries for " ++ show s ++ " (right)") (se1,se1')

  str <- D.toList dly
  forM_ (M.toList $ books^.MB.byMut) $
    \(mut, ME _ _ ddns dnm cis@(CIs mutJT _ _ _)) ->
      case M.lookup mut (books'^.MB.byMut) of
        Nothing -> err' ("extra mut entry found: " ++ pShow mut)
        Just (ME _ _ ddns' dnm' cis') -> do
          when (ddns /= ddns') $
            err' $ "fields ddns for mut entry " ++ pShow mut
            ++ "\ndon't check out: " ++ pShow (ddns, ddns')
            ++ "\n\n" ++ pShowStrMut mut mutJT jt str
          when (dnm /= dnm') $
            err' $ "fields dnm for mut entry " ++ pShow mut
            ++ "\ndon't check out: " ++ pShow (dnm, dnm')
            ++ "\n\n" ++ pShowStrMut mut mutJT jt str
          return $ validateCIs
            ("Run.validate: mismatch in CIs for mut "
             ++ pShow mut ++ ": ") cis cis'

  forM_ (M.toList $ books'^.MB.byMut) $ \(mut, _) ->
    case M.lookup mut (books^.MB.byMut) of
      Nothing -> err' ("missing mut entry: " ++ pShow mut)
      Just _  -> return ()

  where
    CIs jt _ _ _ = memCIs
    m = U.length ns
    err' = err . ("validate: " ++)
    err'' name vals = err' $ name ++ " don't check out:\n"
                      ++ pShow vals

validateCIs :: String -> CIs -> CIs -> ()
validateCIs msg (CIs jt ndns bhd btl) (CIs jt' ndns' bhd' btl')
  | jt /= jt' = err' "joint types" (jt,jt')
  | ndns /= ndns' = err' "joint counts (individual)" $ imValDiff ndns ndns'
  | bhd /= bhd' = err' "CIs (by head)" $ imKeyDiff bhd bhd'
  | btl /= btl' = err' "CIs (by tail)" $ imKeyDiff btl btl'
  | otherwise = ()
  where
    err' name = err . ((msg ++ name ++ " don't check out: ") ++) . show
    imKeyDiff im im' = (im IM.\\ im', im' IM.\\ im)
    imValDiff im im' = IM.unionWith (+) im (negate <$> im')

err :: String -> a
err = error . ("Run." ++)
