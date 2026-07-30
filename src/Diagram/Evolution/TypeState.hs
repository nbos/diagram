{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns, LambdaCase, TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module Diagram.Evolution.TypeState (module Diagram.Evolution.TypeState) where

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import Data.Maybe
import qualified Data.List as L
import Data.Strict.Tuple (Pair((:!:)),(:!:))
import qualified Data.Strict.Tuple as Strict
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Mutable as MV

import Diagram.Primitive

import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import Diagram.String
import qualified Diagram.Doubly as D
import Diagram.ConstrInterval(CI(..))
import qualified Diagram.ConstrInterval as CI

import Diagram.Evolution.Mutation (Mutation(..))
import Diagram.Evolution.SymEntry ( SymEntry(SymEntry, _isMember, _coSymsIn),
                                    coSymsIn, coSymsOut, dependents, isMember )
import qualified Diagram.Evolution.SymEntry as Sym

import Diagram.Util

----------------------
-- JOINT TYPE STATE --
----------------------

type TypeT m = StateT (TypeState (PrimState m)) m
data TypeState s = TS
  { _jointType :: !JointType -- :: (IntSet, IntSet)
  , _leftSyms  :: !(MV.MVector s SymEntry)
  , _rightSyms :: !(MV.MVector s SymEntry) }
makeLenses ''TypeState

-- | m
numSymbols :: Monad m => TypeT m Int
numSymbols = leftSyms `uses` MV.length

-- | (sz0, sz1)
dims :: Monad m => TypeT m (Int, Int)
dims = jointType `uses` JT.dims

-- | vm = sz0 * sz1
variety :: Monad m => TypeT m Int
variety = jointType `uses` JT.variety

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
leftMember (TS _ u0 _) s = _isMember <$> MV.read u0 s

rightMember :: PrimMonad m => TypeState (PrimState m) -> Sym -> m Bool
rightMember (TS _ _ u1) s = _isMember <$> MV.read u1 s

member :: PrimMonad m => TypeState (PrimState m) -> Sym -> Sym -> m Bool
member ts s0 s1 = liftA2 (&&) (leftMember ts s0) (rightMember ts s1)

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the given joint in the type
mutsOf :: PrimMonad m =>
          TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
mutsOf (TS _ u0 u1) s0 s1 = Sym.mutsOf <$> sequence (s0, MV.read u0 s0)
                                       <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly missing) mutation that would make the given
-- joint member of the type (assumes it's not)
addMutOf :: PrimMonad m =>
            TypeState (PrimState m) -> Sym -> Sym -> m (Maybe Mutation)
addMutOf (TS _ u0 u1) s0 s1 = Sym.addMutOf <$> sequence (s0, MV.read u0 s0)
                                           <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly empty) set of available Del mutations that would
-- take the given joint out of the type (assumes it's in)
delMutsOf :: PrimMonad m =>
             TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
delMutsOf (TS _ u0 u1) s0 s1 = Sym.delMutsOf <$> sequence (s0, MV.read u0 s0)
                                             <*> sequence (s1, MV.read u1 s1)

----------
-- INIT --
----------

-- | Given the number of symbols, a list of all joints and a joint type,
-- return the SymEntries of the left and right unions of the type
init :: PrimMonad m => Int -> [(Sym,Sym)] -> JointType ->
        m (TypeState (PrimState m))
init m allJoints jt@(JT u0 u1) = do
  uLeft  <- MV.replicate m Sym.emptyOut -- uLeft
  uRight <- MV.replicate m Sym.emptyOut -- uRight
  forM_ s0s $ flip (MV.write uLeft ) Sym.emptyIn
  forM_ s1s $ flip (MV.write uRight) Sym.emptyIn

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

  return $ TS jt uLeft uRight
  where
    s0s = UT.toList u0 -- left member symbols
    s1s = UT.toList u1 -- right member symbols

------------
-- UPDATE --
------------

-- | (Read only) Return the Mutations to be added (fst) or removed (snd)
-- from the Books after a given Mutation is applied. This must be called
-- *before* applying the mutation.
deltaMut :: forall m.
  PrimMonad m => Mutation -> TypeT m ([Mutation], [Mutation])
deltaMut mut = fmap (Strict.uncurry (,)) $ case mut of

  AddLeft s0 -> flip execStateT ([DelLeft s0] :!: [mut]) $ do
    SymEntry _ coIn0 _ coOut0 <- lift $ readLeft s0
    forM_ (IS.toList coOut0) addAddRightsFromAddLeft --

    whenJust (trySingleton coIn0) $ \s1 -> do
      SymEntry _ _ deps1 _ <- lift $ readRight s1
      when (IS.null deps1) $ delMut (DelRight s1) --

    depsLost <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn0) $ \s1 -> do
        SymEntry _ coIn1 _ _ <- lift $ readRight s1
        return $ trySingleton coIn1 <&> (, IS.singleton s1)
    forM_ (IM.toList depsLost) $ \(s0', deps) -> do
      SymEntry _ coIn0' deps0' _ <- lift $ readLeft s0'
      whenJust (trySingleton coIn0') $ \s1 ->
        delMut (Del2 s0' s1) --
      let lostAllDeps = deps0' == deps
      when lostAllDeps $ do
        addMut (DelLeft s0') --

  -- symmetric w/ above
  AddRight s1 -> flip execStateT ([DelRight s1] :!: [mut]) $ do
    SymEntry _ coIn1 _ coOut1 <- lift $ readRight s1
    forM_ (IS.toList coOut1) addAddLeftsFromAddRight --

    whenJust (trySingleton coIn1) $ \s0 -> do
      SymEntry _ _ deps0 _ <- lift $ readLeft s0
      when (IS.null deps0) $ delMut (DelLeft s0) --

    depsLost <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn1) $ \s0 -> do
        SymEntry _ coIn0 _ _ <- lift $ readLeft s0
        return $ trySingleton coIn0 <&> (, IS.singleton s0)
    forM_ (IM.toList depsLost) $ \(s1', deps) -> do
      SymEntry _ coIn1' deps1' _ <- lift $ readRight s1'
      whenJust (trySingleton coIn1') $ \s0 ->
        delMut (Del2 s0 s1') --
      let lostAllDeps = deps1' == deps
      when lostAllDeps $ do
        addMut (DelRight s1') --

  Add2 s0 s1 -> flip execStateT ([Del2 s0 s1] :!: [mut]) $ do
    SymEntry _ _ _ coOut0 <- lift $ readLeft s0
    forM_ (IS.toList $ IS.delete s1 coOut0) addAddRightsFromAddLeft --
    SymEntry _ _ _ coOut1 <- lift $ readRight s1
    forM_ (IS.toList $ IS.delete s0 coOut1) addAddLeftsFromAddRight --

  DelLeft s0 -> flip execStateT ([AddLeft s0] :!: [mut]) $ do
    SymEntry _ coIn0 _ coOut0 <- lift $ readLeft s0
    forM_ (IS.toList coOut0) delAddRightsFromDelLeft --

    whenJust (trySingleton coIn0) $ \s1 -> do
      SymEntry _ _ deps1 _ <- lift $ readRight s1
      when (deps1 == IS.singleton s0) $ addMut (DelRight s1) --

    depsGained <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn0) $ \s1 -> do
        SymEntry _ coIn1 _ _ <- lift $ readRight s1
        return $ trySingleton (IS.delete s0 coIn1) <&> (, IS.singleton s1)
    forM_ (IM.toList depsGained) $ \(s0', deps) -> do
      SymEntry _ coIn0' deps0' _ <- lift $ readLeft s0'
      when (IS.null deps0') $ do
        delMut (DelLeft s0') --
        whenJust (trySingleton deps) $ \s1 -> do
          when (coIn0' == IS.singleton s1) $
            addMut (Del2 s0' s1) --

  -- symmetric w/ above
  DelRight s1 -> flip execStateT ([AddRight s1] :!: [mut]) $ do
    SymEntry _ coIn1 _ coOut1 <- lift $ readRight s1
    forM_ (IS.toList coOut1) delAddLeftsFromDelRight --

    whenJust (trySingleton coIn1) $ \s0 -> do
      SymEntry _ _ deps0 _ <- lift $ readLeft s0
      when (deps0 == IS.singleton s1) $ addMut (DelLeft s0) --

    depsGained <- fmap (IM.fromListWith IS.union . catMaybes) $
      forM (IS.toList coIn1) $ \s0 -> do
        SymEntry _ coIn0 _ _ <- lift $ readLeft s0
        return $ trySingleton (IS.delete s1 coIn0) <&> (, IS.singleton s0)
    forM_ (IM.toList depsGained) $ \(s1', deps) -> do
      SymEntry _ coIn1' deps1' _ <- lift $ readRight s1'
      when (IS.null deps1') $ do
        delMut (DelRight s1') --
        whenJust (trySingleton deps) $ \s0 -> do
          when (coIn1' == IS.singleton s0) $
            addMut (Del2 s0 s1') --

  Del2 s0 s1 -> flip execStateT ([Add2 s0 s1] :!: [mut]) $ do
    SymEntry _ _ _ coOut0 <- lift $ readLeft s0
    forM_ (IS.toList coOut0) delAddRightsFromDelLeft --
    SymEntry _ _ _ coOut1 <- lift $ readRight s1
    forM_ (IS.toList coOut1) delAddLeftsFromDelRight --

  where
    addMut :: Mutation -> StateT ([Mutation] :!: [Mutation]) (TypeT m) ()
    addMut mt = _1 %= (mt:)
    delMut :: Mutation -> StateT ([Mutation] :!: [Mutation]) (TypeT m) ()
    delMut mt = _2 %= (mt:)

    -- | Add an `AddRight s1` mutation made available by the
    -- introduction of a neighbor `s0` to the left union
    addAddRightsFromAddLeft s1 = do
      SymEntry _ coIn1 _ coOut1 <- lift $ readRight s1
      when (IS.null coIn1) $ do
        addMut (AddRight s1) --
        forM_ (IS.toList coOut1) $ \s0' -> do
          SymEntry _ coIn0' _ _ <- lift $ readLeft s0'
          when (IS.null coIn0') $ delMut (Add2 s0' s1) --

    -- | Delete `AddRight s1` mutations invalidated from the deltion of
    -- its last left in-neighbor
    delAddRightsFromDelLeft s1 = do
      SymEntry _ coIn1 _ coOut1 <- lift $ readRight s1
      whenJust (trySingleton coIn1) $ \_ -> do
        delMut (AddRight s1) --
        forM_ (IS.toList coOut1) $ \s0' -> do
          SymEntry _ coIn0' _ _ <- lift $ readLeft s0'
          when (IS.null coIn0') $ addMut (Add2 s0' s1) --

    -- | Add an `AddLeft s0` mutation made available by the introduction
    -- of a neighbor `s1` to the left union
    addAddLeftsFromAddRight s0 = do
      SymEntry _ coIn0 _ coOut0 <- lift $ readLeft s0
      when (IS.null coIn0) $ do
        addMut (AddLeft s0) --
        forM_ (IS.toList coOut0) $ \s1' -> do
          SymEntry _ coIn1' _ _ <- lift $ readRight s1'
          when (IS.null coIn1') $ delMut (Add2 s0 s1') --

    -- | Delete `AddLeft s0` mutations invalidated from the deltion of
    -- its last right in-neighbor
    delAddLeftsFromDelRight s0 = do
      SymEntry _ coIn0 _ coOut0 <- lift $ readLeft s0
      whenJust (trySingleton coIn0) $ \_ -> do
        delMut (AddLeft s0) --
        forM_ (IS.toList coOut0) $ \s1' -> do
          SymEntry _ coIn1' _ _ <- lift $ readRight s1'
          when (IS.null coIn1') $ addMut (Add2 s0 s1') --

err :: String -> a
err = error . ("TypeState." ++)

-- | Apply a mutation to the type state
pushMut :: PrimMonad m => Mutation -> TypeT m ()
pushMut = \case
  AddLeft s0 -> addLeft s0
  AddRight s1 -> addRight s1
  Add2 s0 s1 -> addLeft s0 >> addRight s1

  DelLeft s0 -> do
    e0 <- readLeft s0
    unless (IS.null $ e0^.dependents) $
      err' $ "delLeft: can't del sym with deps: " ++ show (s0, e0^.dependents)
    delLeft s0 e0

  DelRight s1 -> do
    e1 <- readRight s1
    unless (IS.null $ e1^.dependents) $
      err' $ "delRight: can't del sym with deps: " ++ show (s1, e1^.dependents)
    delRight s1 e1

  Del2 s0 s1 -> do -- co-deps
    e0 <- readLeft s0
    e1 <- readRight s1
    delLeft s0 e0
    delRight s1 e1
  where
    addLeft s0 = do
      jointType %= JT.insertLeftMissing s0
      e0@(SymEntry mem coIn deps coOut) <- readLeft s0
      when mem $ err' $ "addLeft: symbol already member: " ++ show s0
      unless (IS.null deps) $
        err' $ "addLeft: out-sym shouldn't have deps: " ++ show (s0,deps)

      -- case: mark s0 as dependent to dependor
      whenJust (trySingleton coIn) $ modifyRight $
          dependents %~ IS.insert s0

      -- unset as Out, set as In, for all neighbors
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyRight $ (coSymsIn  %~ IS.insert s0)
                    . (coSymsOut %~ IS.delete s0)

      writeLeft s0 $ e0 & isMember .~ True

    -- FIXME: missing some dependency deleting

    addRight s1 = do
      jointType %= JT.insertRightMissing s1
      e1@(SymEntry mem coIn deps coOut) <- readRight s1
      when mem $ err' $ "addRight: symbol already member: " ++ show s1
      unless (IS.null deps) $
        err' $ "addRight: out-sym shouldn't have deps: " ++ show (s1,deps)
      whenJust (trySingleton coIn) $ modifyLeft $
          dependents %~ IS.insert s1 -- mark s1 as dependent to _
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyLeft $ \e0 -> e0 & coSymsIn  %~ IS.insert s1
                               & coSymsOut %~ IS.delete s1
      writeRight s1 $ e1 & isMember .~ True

    delLeft s0 e0@(SymEntry mem coIn _ coOut) = do
      jointType %= JT.deleteLeftMember s0
      unless mem $ err' $ "delLeft: symbol not member: " ++ show s0

      forM_ (IS.toList coIn) $ \s1 -> do
        e1 <- readRight s1
        let e1' = e1 & coSymsIn  %~ IS.delete s0
                     & coSymsOut %~ IS.insert s0
        whenJust (trySingleton $ e1'^.coSymsIn) $ modifyLeft $
          dependents %~ IS.insert s1 -- mark s1 as dependent to _
        writeRight s1 e1'

      forM_ (IS.toList coOut) $
        modifyRight $ \e1 -> e1 & coSymsIn  %~ IS.delete s0
                                & coSymsOut %~ IS.insert s0

      writeLeft s0 $ e0 & isMember .~ False

    delRight s1 e1@(SymEntry mem coIn _ coOut) = do
      jointType %= JT.deleteRightMember s1
      unless mem $ err' $ "delRight: symbol not member: " ++ show s1

      forM_ (IS.toList coIn) $ \s0 -> do
        e0 <- readLeft s0
        let e0' = e0 & coSymsIn  %~ IS.delete s1
                     & coSymsOut %~ IS.insert s1
        whenJust (trySingleton $ e0'^.coSymsIn) $ modifyRight $
          dependents %~ IS.insert s0 -- mark s0 as dependent to _
        writeLeft s0 e0'

      forM_ (IS.toList coOut) $
        modifyLeft $ (coSymsIn  %~ IS.delete s1)
                   . (coSymsOut %~ IS.insert s1)

      writeRight s1 $ e1 & isMember .~ False

    err' = err . ("pushMut: " ++)

trySingleton :: IntSet -> Maybe Sym
trySingleton is | [s] <- IS.toList is = Just s
                | otherwise = Nothing

--------------------------
-- STRING/CI OPERATIONS --
--------------------------

-- | Break a constructive interval of the joint type (in) into an
-- ordered (by tail) list of its segments by mutation.
decomposeIn :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m [(Mutation, CI)]
decomposeIn str tst ci@(CI hd shd len tl _)
  | len == 2  = (,ci) <<$>> delMutsOf tst hd tl
  | otherwise = go [] hd shd . tail =<< CI.extension str ci
  where
    go mcis _ _ [] = return mcis
    go mcis i0 s0 ((i1,s1):rest) = do
      muts <- delMutsOf tst s0 s1
      let (alive, ended) = L.partition (flip elem muts . fst) mcis
          started = (, CI i0 s0 2 i1 s1) <$>
                    filter (`notElem` (fst <$> mcis)) muts
          mcis' = (++ started) $ (<<$>> alive) $ \c ->
            c{ _ciLength = _ciLength c + 1
             , _tailIndex = i1
             , _tailSymbol = s1 }
      (ended ++) <$> go mcis' i1 s1 rest

-- | Return the out-interval (and the add-mutation that would switch its
-- membership) immediately preceding the given in-interval but only if
-- the out-interval is not itself preceded by another in-interval (will
-- get caught by nextMutCIs instead). Returns `Nothing` if the preceding
-- interval is so sandwitched, `Just Nothing` if there is either no
-- preceding joint (begining of the string) or if it not add-able, and
-- `Just Just` if there is such a mutation-interval pair.
prevMutCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
  TypeState (PrimState m) -> CI -> m (Maybe (Maybe (Mutation, CI)))
prevMutCI str tst (CI tl stl _ _ _) = (D.prev str tl >>=) $ \case
  Nothing -> return $ Just Nothing -- no prev symbol/interval
  Just (ptl,sptl) -> (addMutOf tst sptl stl >>=) $ \case
    Nothing -> return $ Just Nothing -- no mut
    Just mut -> (mut,) <<<$>>> go 2 ptl sptl
      where
        go !len hd shd = (D.prev str hd >>=) $ \case
          Nothing -> return $ Just $ Just ci -- hit start, end
          Just (phd,sphd) -> (member tst sphd shd >>=) $ \case
            True -> return Nothing -- not first of a chain (cancel)
            False -> (addMutOf tst sphd shd >>=) $ \case
              Just mut' | mut' == mut -> go (len+1) phd sphd
              _else -> return $ Just $ Just ci -- end of interval
          where
            ci = CI hd shd len tl stl

-- | For a string, a (joint-)type state, a joint type, and a continuous
-- interval of joints member of the given joint type, but not of the one
-- represented in the state, return the CI strictly greater than the one
-- given---if it exists---which is member of the union of the state's
-- and the given joint type, but only if this super-CI doesn't contain
-- another CI member of the given joint type on the left of the given
-- CI. This way a mapMaybe over a set of CIs will return a set of
-- super-CIs.
superCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
           TypeState (PrimState m) -> JointType -> CI -> m (Maybe CI)
superCI dly tst jt (CI hd0 shd0 len0 tl0 stl0) = do

  bwd <- (D.prev dly hd0 >>=) $ \case
    Nothing -> return Nothing
    Just (phd, sphd) -> (member tst sphd shd0 >>=) $ \case
      False -> return Nothing
      True -> Just <$> goBwd hd0 shd0 phd sphd 2 -- tl first

  fwd <- (D.next dly tl0 >>=) $ \case
    Nothing -> return Nothing
    Just (ntl, sntl) -> (member tst stl0 sntl >>=) $ \case
      False -> return Nothing
      True -> Just <$> goFwd tl0 stl0 2 ntl sntl

  return $ case (bwd,fwd) of
    (Nothing, Nothing) -> Nothing
    (Just (CI hd shd lenBwd _ _), Nothing) ->
      let len = lenBwd + len0 - 1
      in Just $ CI hd shd len tl0 stl0

    (Nothing, Just (CI _ _ lenFwd tl stl)) ->
      let len = len0 + lenFwd - 1
      in Just $ CI hd0 shd0 len tl stl

    (Just (CI hd shd lenBwd _ _), Just (CI _ _ lenFwd tl stl)) ->
      let len = lenBwd + len0 + lenFwd - 2
      in Just $ CI hd shd len tl stl

  where
    goBwd tl stl = go
      where
        go hd shd !len = (D.prev dly hd >>=) $ \case
          Nothing -> return ci -- eos
          Just (phd, sphd) -> (member tst sphd shd >>=) $ \case
            False -> return ci -- end
            True -> go phd sphd (len+1)
          where ci = CI hd shd len tl stl

    goFwd hd shd = goST
      where
        goST !len tl stl = (D.next dly tl >>=) $ \case
          Nothing -> return ci -- eos
          Just (ntl, sntl) -> (member tst stl sntl >>=) $ \case
            True -> goST (len+1) ntl sntl
            False | JT.member (stl,sntl) jt -> goJT (len+1) ntl sntl
                  | otherwise -> return ci -- end
          where ci = CI hd shd len tl stl

        goJT !len tl stl = (D.next dly tl >>=) $ \case
          Nothing -> return ci -- eos
          Just (ntl, sntl)
            | JT.member (stl,sntl) jt -> goJT (len+1) ntl sntl
            | otherwise -> (member tst stl sntl >>=) $ \case
                True -> goST (len+1) ntl sntl
                False -> return ci -- end
          where ci = CI hd shd len tl stl


-- | Given the string, joint type and a constructive interval of the
-- joint type (a.k.a. in-interval), return the longest immediately
-- following sequence of alternating out-, int-, out-, etc. intervals
-- where all the out-intervals would get their membership flipped
-- (i.e. included) by the same add-mutation, which is also
-- returned. Return Nothing if end of string or if the following joint
-- does not have an add-mutation.
nextMutCIs :: forall m. PrimMonad m => Doubly (PrimState m) ->
              TypeState (PrimState m) -> CI -> m (Maybe (Mutation, [CI]))
nextMutCIs str tst (CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (addMutOf tst s0 s1 >>=) $ \case
    Nothing -> return Nothing -- no add-mutation
    Just addMut -> Just . (addMut,) <$> grabOut [] (CI s0 i0) 2 i1 s1
      where
        grabOut :: [CI] -> (Len -> Index -> Sym -> CI) ->
                   Len -> Index -> Sym -> m [CI]
        grabOut acc mkCI !len tl stl = (D.next str tl >>=) $ \case
          Nothing -> return $ reverse acc' -- hit end of string
          Just (ntl,sntl) -> (member tst stl sntl >>=) $ \case
            True -> grabIn acc' (CI tl stl) 2 ntl sntl -- switch
            False -> (addMutOf tst stl sntl >>=) $ \case
              Just addMut' | addMut' == addMut ->
                grabOut acc mkCI (len+1) ntl sntl -- keep going
              _else -> return $ reverse acc' -- end of intervals
          where
            acc' = mkCI len tl stl : acc

        grabIn :: [CI] -> (Len -> Index -> Sym -> CI) ->
                  Len -> Index -> Sym -> m [CI]
        grabIn acc mkCI !len tl stl = (D.next str tl >>=) $ \case
          Nothing -> return $ reverse acc' -- hit end of string
          Just (ntl,sntl) -> (member tst stl sntl >>=) $ \case
            True -> grabIn acc mkCI (len+1) ntl sntl -- keep going
            False -> (addMutOf tst stl sntl >>=) $ \case
              Just addMut' | addMut' == addMut ->
                grabOut acc' (CI tl stl) 2 ntl sntl -- switch
              _else -> return $ reverse acc' -- end of intervals
          where
            acc' = mkCI len tl stl : acc
