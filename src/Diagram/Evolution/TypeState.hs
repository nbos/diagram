{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns, LambdaCase, TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.Evolution.TypeState (module Diagram.Evolution.TypeState) where

import Debug.Trace

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Diagram.Pretty
import Diagram.Primitive

import Diagram.Joints (Joints)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import Diagram.ConstrIntervals (CIs)
import Diagram.String

import Diagram.Evolution.Mutation (Mutation(..))
import qualified Diagram.Evolution.Mutation as Mut
import Diagram.Evolution.SymEntry ( SymEntry(SE, _isMember, _coSymsIn),
                                    coSymsIn, coSymsOut, dependents, isMember )
import qualified Diagram.Evolution.SymEntry as SE

import Diagram.Util

----------------------
-- JOINT TYPE STATE --
----------------------

type TypeT m = StateT (TypeState (PrimState m)) m
data TypeState s = TS
  { _leftSyms  :: !(MV.MVector s SymEntry)   -- :: sym -> mem
  , _rightSyms :: !(MV.MVector s SymEntry) } -- :: sym -> mem
makeLenses ''TypeState

-- | m
numSymbols :: Monad m => TypeT m Int
numSymbols = leftSyms `uses` MV.length

-- | Clones state
clone :: PrimMonad m => TypeState (PrimState m) -> m (TypeState (PrimState m))
clone (TS u0 u1) = TS <$> MV.clone u0 <*> MV.clone u1

-- | Assuming the mutation is valid/available, return the set of joints
-- that will flip membership upon its application.
jointsOf :: PrimMonad m => TypeState (PrimState m) -> Mutation -> m (Joints ())
jointsOf ts mut = case mut of
  AddLeft s0  -> goLeft s0
  AddRight s1 -> goRight s1
  Add2 s0 s1  -> return $ M.singleton (s0,s1) ()
  DelLeft s0  -> goLeft s0
  DelRight s1 -> goRight s1
  Del2 s0 s1  -> return $ M.singleton (s0,s1) ()
  where
    goLeft s0  = M.fromDistinctAscList
                 . fmap ((,()) . (s0,))
                 . IS.toAscList
                 . SE._coSymsIn <$> readLeft_ ts s0
    goRight s1 = M.fromDistinctAscList
                 . fmap ((,()) . (,s1))
                 . IS.toAscList
                 . SE._coSymsIn <$> readRight_ ts s1

-----------------
-- BOILERPLATE --
-----------------

-- READ/WRITE

readLeft :: PrimMonad m => Sym -> TypeT m SymEntry
readLeft s = use leftSyms >>= lift . flip MV.read s

readLeft_ :: PrimMonad m => TypeState (PrimState m) -> Sym -> m SymEntry
readLeft_ = MV.read . _leftSyms

readRight :: PrimMonad m => Sym -> TypeT m SymEntry
readRight s = use rightSyms >>= lift . flip MV.read s

readRight_ :: PrimMonad m => TypeState (PrimState m) -> Sym -> m SymEntry
readRight_ = MV.read . _rightSyms

writeLeft :: PrimMonad m => Sym -> SymEntry -> TypeT m ()
writeLeft s e = use leftSyms >>= lift . flip2 MV.write s e

writeRight :: PrimMonad m => Sym -> SymEntry -> TypeT m ()
writeRight s e = use rightSyms >>= lift . flip2 MV.write s e

modifyLeft :: PrimMonad m => (SymEntry -> SymEntry) ->
              Sym -> TypeT m ()
modifyLeft f s = use leftSyms >>= lift . flip2 MV.modify f s

modifyRight :: PrimMonad m => (SymEntry -> SymEntry) ->
               Sym -> TypeT m ()
modifyRight f s = use rightSyms >>= lift . flip2 MV.modify f s

-- PREDICATES

leftMember :: PrimMonad m => TypeState (PrimState m) -> Sym -> m Bool
leftMember (TS u0 _) s = _isMember <$> MV.read u0 s

rightMember :: PrimMonad m => TypeState (PrimState m) -> Sym -> m Bool
rightMember (TS _ u1) s = _isMember <$> MV.read u1 s

member :: PrimMonad m => TypeState (PrimState m) -> Sym -> Sym -> m Bool
member ts s0 s1 = liftA2 (&&) (leftMember ts s0) (rightMember ts s1)

-- RELATIONS

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the given joint in the type
mutsOf :: PrimMonad m =>
          TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
mutsOf (TS u0 u1) s0 s1 = SE.mutsOf <$> sequence (s0, MV.read u0 s0)
                                    <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly missing) mutation that would make the given
-- joint member of the type (assumes it's not)
addMutOf :: PrimMonad m =>
            TypeState (PrimState m) -> Sym -> Sym -> m (Maybe Mutation)
addMutOf (TS u0 u1) s0 s1 = SE.addMutOf <$> sequence (s0, MV.read u0 s0)
                                        <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly empty) set of available Del mutations that would
-- take the given joint out of the type (assumes it's in)
delMutsOf :: PrimMonad m =>
             TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
delMutsOf (TS u0 u1) s0 s1 = SE.delMutsOf <$> sequence (s0, MV.read u0 s0)
                                          <*> sequence (s1, MV.read u1 s1)

----------
-- INIT --
----------

-- | Given the number of symbols, a list of all joints and a joint type,
-- return the SymEntries of the left and right unions of the type
init :: PrimMonad m => Int -> [(Sym,Sym)] -> JointType ->
        m (TypeState (PrimState m))
init m allJoints (JT u0 u1) = do
  uLeft  <- MV.replicate m SE.emptyOut -- uLeft
  uRight <- MV.replicate m SE.emptyOut -- uRight
  forM_ s0s $ flip (MV.write uLeft ) SE.emptyIn
  forM_ s1s $ flip (MV.write uRight) SE.emptyIn

  -- cosyms (in/out)
  forM_ allJoints $ \(s0,s1) -> do
    se0 <- MV.read uLeft s0
    se1 <- MV.read uRight s1
    MV.write uLeft s0 $ se0 & if se1^.isMember
      then coSymsIn %~ IS.insert s1
      else coSymsOut %~ IS.insert s1
    MV.write uRight s1 $ se1 & if se0^.isMember
      then coSymsIn %~ IS.insert s0
      else coSymsOut %~ IS.insert s0

  -- deps
  forM_ s0s $ \s0 -> do
    ic0s <- _coSymsIn <$> MV.read uLeft s0
    case IS.toList ic0s of
      [s1] -> MV.modify uRight (dependents %~ IS.insert s0) s1
      _else -> return ()

  forM_ s1s $ \s1 -> do
    ic1s <- _coSymsIn <$> MV.read uRight s1
    case IS.toList ic1s of
      [s0] -> MV.modify uLeft (dependents %~ IS.insert s1) s0
      _else -> return ()

  return $ TS uLeft uRight
  where
    s0s = UT.toList u0 -- left member symbols
    s1s = UT.toList u1 -- right member symbols

------------
-- UPDATE --
------------

data DeltaMutJointsState = DMJS
  { _enabledMuts :: !(Set Mutation)
  , _expiredMuts :: !(Set Mutation)
  , _addedJoints :: !(Map Mutation CIs)
  , _deletedJoints :: !(Map Mutation CIs) }
  deriving (Show,Eq)
makeLenses ''DeltaMutJointsState

-- | Return the Mutations to be added (fst) or removed (snd) from the
-- Books after a given Mutation is applied. This must be called
-- **before** applying the mutation.
deltaMutJoints :: forall m. PrimMonad m => TypeState (PrimState m) ->
                  Mutation -> m (Set Mutation, Set Mutation)
deltaMutJoints tst mut = fmap toLazy $ flip execStateT st0 $ case mut of

  AddLeft s0 -> do
    SE _ coIn _ coOut <- readL s0
    forM_ (IS.toList coOut) addAddRightsFromAddLeft --

    whenJust (trySingleton coIn) $ \s1 -> do
      SE _ _ deps1 _ <- readR s1
      when (IS.null deps1) $ delMut (DelRight s1) --

    depsLost <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn) $ \s1 -> do
        SE _ coIn1 _ _ <- readR s1
        return $ trySingleton coIn1 <&> (, IS.singleton s1)
    forM_ (IM.toList depsLost) $ \(s0', deps) -> do
      SE _ coIn0' deps0' _ <- readL s0'
      whenJust (trySingleton coIn0') $ \s1 ->
        delMut (Del2 s0' s1) --
      let lostAllDeps = deps0' == deps
      when lostAllDeps $ do
        addMut (DelLeft s0') --

  -- symmetric w/ above
  AddRight s1 -> do
    SE _ coIn _ coOut <- readR s1
    forM_ (IS.toList coOut) addAddLeftsFromAddRight --

    whenJust (trySingleton coIn) $ \s0 -> do
      SE _ _ deps0 _ <- readL s0
      when (IS.null deps0) $ delMut (DelLeft s0) --

    depsLost <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn) $ \s0 -> do
        SE _ coIn0 _ _ <- readL s0
        return $ trySingleton coIn0 <&> (, IS.singleton s0)
    forM_ (IM.toList depsLost) $ \(s1', deps) -> do
      SE _ coIn1' deps1' _ <- readR s1'
      whenJust (trySingleton coIn1') $ \s0 ->
        delMut (Del2 s0 s1') --
      let lostAllDeps = deps1' == deps
      when lostAllDeps $ do
        addMut (DelRight s1') --

  Add2 s0 s1 -> do
    SE _ _ _ coOut0 <- readL s0
    forM_ (IS.toList $ IS.delete s1 coOut0) addAddRightsFromAddLeft --
    SE _ _ _ coOut1 <- readR s1
    forM_ (IS.toList $ IS.delete s0 coOut1) addAddLeftsFromAddRight --

  DelLeft s0 -> do
    SE _ coIn _ coOut <- readL s0
    forM_ (IS.toList coOut) delAddRightsFromDelLeft --

    whenJust (trySingleton coIn) $ \s1 -> do
      SE _ _ deps1 _ <- readR s1
      when (deps1 == IS.singleton s0) $ addMut (DelRight s1) --

    depsGained <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn) $ \s1 -> do
        SE _ coIn1 _ _ <- readR s1
        return $ trySingleton (IS.delete s0 coIn1) <&> (, IS.singleton s1)
    forM_ (IM.toList depsGained) $ \(s0', deps) -> do
      SE _ coIn0' deps0' _ <- readL s0'
      when (IS.null deps0') $ do
        delMut (DelLeft s0') --
        whenJust (trySingleton deps) $ \s1 -> do
          when (coIn0' == IS.singleton s1) $
            addMut (Del2 s0' s1) --

  -- symmetric w/ above
  DelRight s1 -> do
    SE _ coIn _ coOut <- readR s1
    forM_ (IS.toList coOut) delAddLeftsFromDelRight --

    whenJust (trySingleton coIn) $ \s0 -> do
      SE _ _ deps0 _ <- readL s0
      when (deps0 == IS.singleton s1) $ addMut (DelLeft s0) --

    depsGained <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn) $ \s0 -> do
        SE _ coIn0 _ _ <- readL s0
        return $ trySingleton (IS.delete s1 coIn0) <&> (, IS.singleton s0)
    forM_ (IM.toList depsGained) $ \(s1', deps) -> do
      SE _ coIn1' deps1' _ <- readR s1'
      when (IS.null deps1') $ do
        delMut (DelRight s1') --
        whenJust (trySingleton deps) $ \s0 -> do
          when (coIn1' == IS.singleton s0) $
            addMut (Del2 s0 s1') --

  Del2 s0 s1 -> do
    SE _ _ _ coOut0 <- readL s0
    forM_ (IS.toList coOut0) delAddRightsFromDelLeft --
    SE _ _ _ coOut1 <- readR s1
    forM_ (IS.toList coOut1) delAddLeftsFromDelRight --

  where
    readL = readLeft_ tst
    readR = readRight_ tst

    addMut :: Mutation -> StateT DeltaMutJointsState m ()
    addMut mu = enabledMuts %= Set.insert mu
    delMut :: Mutation -> StateT DeltaMutJointsState m ()
    delMut mu = expiredMuts %= Set.insert mu

    ss :: Mutation -> Set Mutation
    ss = Set.singleton

    st0 = DMJS (ss $ Mut.recip mut) (ss mut) M.empty M.empty
    toLazy (DMJS m0 m1 _ _) = (m0, m1)

    -- | Add an `AddRight s1` mutation made available by the
    -- introduction of a neighbor `s0` to the left union
    addAddRightsFromAddLeft s1 = do
      SE _ coIn _ coOut1 <- readR s1
      when (IS.null coIn) $ do
        addMut (AddRight s1) --
        forM_ (IS.toList coOut1) $ \s0' -> do
          SE _ coIn0' _ _ <- readL s0'
          when (IS.null coIn0') $ delMut (Add2 s0' s1) --

    -- | Delete `AddRight s1` mutations invalidated from the deltion of
    -- its last left in-neighbor
    delAddRightsFromDelLeft s1 = do
      SE _ coIn _ coOut <- readR s1
      whenJust (trySingleton coIn) $ \_ -> do
        delMut (AddRight s1) --
        forM_ (IS.toList coOut) $ \s0' -> do
          SE _ coIn0' _ _ <- readL s0'
          when (IS.null coIn0') $ addMut (Add2 s0' s1) --

    -- | Add an `AddLeft s0` mutation made available by the introduction
    -- of a neighbor `s1` to the left union
    addAddLeftsFromAddRight s0 = do
      SE _ coIn _ coOut <- readL s0
      when (IS.null coIn) $ do
        addMut (AddLeft s0) --
        forM_ (IS.toList coOut) $ \s1' -> do
          SE _ coIn1' _ _ <- readR s1'
          when (IS.null coIn1') $ delMut (Add2 s0 s1') --

    -- | Delete `AddLeft s0` mutations invalidated from the deltion of
    -- its last right in-neighbor
    delAddLeftsFromDelRight s0 = do
      SE _ coIn _ coOut <- readL s0
      whenJust (trySingleton coIn) $ \_ -> do
        delMut (AddLeft s0) --
        forM_ (IS.toList coOut) $ \s1' -> do
          SE _ coIn1' _ _ <- readR s1'
          when (IS.null coIn1') $ addMut (Add2 s0 s1') --

err :: String -> a
err = error . ("TypeState." ++)

-- | Apply a mutation to the type state
pushMut :: PrimMonad m => Mutation -> TypeT m ()
pushMut = \case
  AddLeft s0 -> do
    SE _ coIn _ _ <- readLeft s0
    when (IS.null coIn) $
      err' $ "AddLeft: need at least one cosym in: " ++ show s0
    addLeft s0

  AddRight s1 -> do
    SE _ coIn _ _ <- readRight s1
    when (IS.null coIn) $
      err' $ "AddRight: need at least one cosym in: " ++ show s1
    addRight s1

  Add2 s0 s1 -> do
    SE _ coIn0 _ _ <- readLeft s0
    SE _ coIn1 _ _ <- readRight s1
    unless (IS.null coIn0 && IS.null coIn1) $
      err' $ "Add2: unatomic, cosym already in: " ++ show ((s0,coIn0)
                                                          ,(s1,coIn1))
    addLeft s0 >> addRight s1

  DelLeft s0 -> do
    SE _ _ deps _ <- readLeft s0
    unless (IS.null deps) $
      err' $ "DelLeft: can't del sym with deps: " ++ show (s0, deps)
    delLeft s0

  DelRight s1 -> do
    SE _ _ deps _ <- readRight s1
    unless (IS.null deps) $
      err' $ "DelRight: can't del sym with deps: " ++ show (s1, deps)
    delRight s1

  Del2 s0 s1 -> do -- co-deps
    SE _ coIn0 _ _ <- readLeft s0
    SE _ coIn1 _ _ <- readRight s1
    unless (coIn0 == IS.singleton s1 && coIn1 == IS.singleton s0) $
      err' $ "Del2: not co-dep: " ++ show ((s0,coIn0)
                                          ,(s1,coIn1))
    delLeft s0 >> delRight s1

  where
    addLeft s0 = do
      SE mem coIn deps coOut <- readLeft s0
      when mem $ err' $ "addLeft: symbol already member: " ++ show s0
      unless (IS.null deps) $
        err' $ "addLeft: out-sym shouldn't have deps: " ++ show (s0,deps)

      -- deps updates through coIn
      forM_ (IS.toList coIn) $ \s1 -> do
        SE _ coIn1 _ _ <- readRight s1
        case IS.toList coIn1 of
          []    -> flip modifyLeft s0  $ dependents %~ IS.insert s1
          [s0'] -> flip modifyLeft s0' $ dependents %~ IS.delete s1
          _else -> return ()

      -- case: mark s0 as dependent to s1
      whenJust (trySingleton coIn) $ modifyRight $
          dependents %~ IS.insert s0

      -- unset as Out, set as In, for all neighbors
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyRight $ (coSymsIn  %~ IS.insert s0)
                    . (coSymsOut %~ IS.delete s0)

      -- commit membership
      flip modifyLeft s0 $ isMember .~ True
      -- jointType %= JT.insertLeftMissing s0

      ------------------------------------

    addRight s1 = do
      SE mem coIn deps coOut <- readRight s1
      when mem $ err' $ "addRight: symbol already member: " ++ show s1
      unless (IS.null deps) $
        err' $ "addRight: out-sym shouldn't have deps: " ++ show (s1,deps)

      -- deps updates through coIn
      forM_ (IS.toList coIn) $ \s0 -> do
        SE _ coIn0 _ _ <- readLeft s0
        case IS.toList coIn0 of
          []    -> flip modifyRight s1  $ dependents %~ IS.insert s0
          [s1'] -> flip modifyRight s1' $ dependents %~ IS.delete s0
          _else -> return ()

      -- case: mark s1 as dependent to s0
      whenJust (trySingleton coIn) $ modifyLeft $
          dependents %~ IS.insert s1

      -- unset as Out, set as In, for all neighbors
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyLeft $ (coSymsIn  %~ IS.insert s1)
                   . (coSymsOut %~ IS.delete s1)

      -- commit membership
      flip modifyRight s1 $ isMember .~ True
      -- jointType %= JT.insertRightMissing s1

      -------------------------------------

    delLeft s0 = do
      SE mem coIn _ coOut <- readLeft s0
      unless mem $ err' $ "delLeft: symbol not member: " ++ show s0

      -- case: remove s0 as dependent to s1
      whenJust (trySingleton coIn) $
        modifyRight (dependents %~ IS.delete s0)

      -- deps updates, set/unset in/out for in-neighbors
      forM_ (IS.toList coIn) $ \s1 -> do
        e1@(SE _ coIn1 _ _) <- readRight s1
        let coIn1' = IS.delete s0 coIn1
        writeRight s1 $ e1 & coSymsIn  .~ coIn1'
                           & coSymsOut %~ IS.insert s0
        case IS.toList coIn1' of
          []    -> flip modifyLeft s0  $ dependents %~ IS.delete s1
          [s0'] -> flip modifyLeft s0' $ dependents %~ IS.insert s1
          _else -> return ()

      -- unset as Out, set as In, for out-neighbors
      forM_ (IS.toList coOut) $
        modifyRight $ (coSymsIn  %~ IS.delete s0)
                    . (coSymsOut %~ IS.insert s0)

      -- commit removal
      flip modifyLeft s0 $ isMember .~ False
      -- jointType %= JT.deleteLeftMember s0

      -----------------------------------

    delRight s1 = do
      SE mem coIn _ coOut <- readRight s1
      unless mem $ err' $ "delRight: symbol not member: " ++ show s1

      -- case: remove s1 as dependent to s0
      whenJust (trySingleton coIn) $
        modifyLeft (dependents %~ IS.delete s1)

      -- deps updates, set/unset in/out for in-neighbors
      forM_ (IS.toList coIn) $ \s0 -> do
        e0@(SE _ coIn0 _ _) <- readLeft s0
        let coIn0' = IS.delete s1 coIn0
        writeLeft s0 $ e0 & coSymsIn  .~ coIn0'
                          & coSymsOut %~ IS.insert s1
        case IS.toList coIn0' of
          []    -> flip modifyRight s1  $ dependents %~ IS.delete s0
          [s1'] -> flip modifyRight s1' $ dependents %~ IS.insert s0
          _else -> return ()

      -- unset as Out, set as In, for out-neighbors
      forM_ (IS.toList coOut) $
        modifyLeft $ (coSymsIn  %~ IS.delete s1)
                   . (coSymsOut %~ IS.insert s1)

      -- commit removal
      flip modifyRight s1 $ isMember .~ False
      -- jointType %= JT.deleteRightMember s1

      ------------------------------------

    err' = err . ("pushMut: " ++)

trySingleton :: IntSet -> Maybe Sym
trySingleton is | [s] <- IS.toList is = Just s
                | otherwise = Nothing

-----------
-- DEBUG --
-----------

pShowTrace :: PrimMonad m => TypeState (PrimState m) -> m ()
pShowTrace (TS u0 u1) = do
  traceM "Printing TypeState:"
  traceM "Left symbols:"
  V.freeze u0 >>= show'
  traceM "\nRight symbols:"
  V.freeze u1 >>= show'
  traceM "\n"
  where
    show' = traceM . pShow
            . filter ((SE.emptyOut /=) . snd)
            . zip [(0::Int)..]
            . V.toList
