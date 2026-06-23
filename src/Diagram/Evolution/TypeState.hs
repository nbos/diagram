{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Diagram.Evolution.TypeState (module Diagram.Evolution.TypeState) where

import Control.Monad
import Control.Monad.Extra
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict

import qualified Data.List as L
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
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

import Diagram.Util

---------------
-- SYM ENTRY --
---------------

data SymEntry = SymEntry
  { _isMember :: !Bool -- ^ True iff self is member of the union type
  , _coSymsIn :: !IntSet -- ^ Symbols that have a joint with
                         -- self and member of the co-union
  , _dependents :: !IntSet -- ^ CoSymsIn that have self as only coSymsIn
  , _coSymsOut :: !IntSet } -- ^ Symbols that have a joint with self and
                            -- *not* member of the co-union
  deriving (Show,Eq,Ord)
makeLenses ''SymEntry

emptyIn :: SymEntry
emptyIn = SymEntry True IS.empty IS.empty IS.empty

emptyOut :: SymEntry
emptyOut = SymEntry False IS.empty IS.empty IS.empty

mutsOf_ :: (Sym, SymEntry) -> (Sym, SymEntry) -> [Mutation]
mutsOf_ se0@(_, SymEntry mem0 _ _ _) se1@(_, SymEntry mem1 _ _ _)
  | mem0, mem1 = delMutsOf_ se0 se1
  | Just mut <- addMutOf_ se0 se1 = [mut]
  | otherwise = []

delMutsOf_ :: (Sym, SymEntry) -> (Sym, SymEntry) -> [Mutation]
delMutsOf_ (s0, SymEntry _ _ d0s _) (s1, SymEntry _ _ d1s _)
  | d0s == IS.singleton s1
  , d1s == IS.singleton s0 = [Del2 s0 s1]
  | otherwise = [ DelLeft s0 | IS.null d0s ]
                ++ [ DelRight s1 | IS.null d1s ]

addMutOf_ :: (Sym, SymEntry) -> (Sym, SymEntry) -> Maybe Mutation
addMutOf_ (s0, SymEntry mem0 ic0s _ _) (s1, SymEntry mem1 ic1s _ _)
  | mem0 = Just $ AddRight s1 -- assert (not mem1)
  | mem1 = Just $ AddLeft s0  -- assert (not mem0)
  | IS.null ic0s && IS.null ic1s = Just $ Add2 s0 s1
  | otherwise = Nothing -- some other mut intros s0 or s1

----------------------
-- JOINT TYPE STATE --
----------------------

type TypeT m = StateT (TypeState (PrimState m)) m
data TypeState s = TS
  { _leftSize  :: !Int
  , _leftType  :: !(MV.MVector s SymEntry)
  , _rightSize :: !Int
  , _rightType :: !(MV.MVector s SymEntry) }
makeLenses ''TypeState

-- | m
numSymbols :: Monad m => TypeT m Int
numSymbols = leftType `uses` MV.length

-- | vm = sz0 * sz1
variety :: Monad m => TypeT m Int
variety = uses2 leftSize rightSize (*)

-- READ/WRITE

readLeft :: PrimMonad m => Sym -> TypeT m SymEntry
readLeft s = use leftType >>= lift . flip MV.read s

readRight :: PrimMonad m => Sym -> TypeT m SymEntry
readRight s = use rightType >>= lift . flip MV.read s

writeLeft :: PrimMonad m => Sym -> SymEntry -> TypeT m ()
writeLeft s e = use leftType >>= lift . flip2 MV.write s e

writeRight :: PrimMonad m => Sym -> SymEntry -> TypeT m ()
writeRight s e = use rightType >>= lift . flip2 MV.write s e

modifyLeft :: PrimMonad m => (SymEntry -> SymEntry) ->
              Sym -> TypeT m ()
modifyLeft f s = use leftType >>= lift . flip2 MV.modify f s

modifyRight :: PrimMonad m => (SymEntry -> SymEntry) ->
               Sym -> TypeT m ()
modifyRight f s = use rightType >>= lift . flip2 MV.modify f s

-- PREDICATES

member :: PrimMonad m => TypeState (PrimState m) -> Sym -> Sym -> m Bool
member (TS _ u0 _ u1) s0 s1 = liftA2 (&&) (_isMember <$> MV.read u0 s0)
                                          (_isMember <$> MV.read u1 s1)

-- | Give the (possibly empty) set of available mutations that would
-- switch the membership of the given joint in the type
mutsOf :: PrimMonad m =>
          TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
mutsOf (TS _ u0 _ u1) s0 s1 = mutsOf_ <$> sequence (s0, MV.read u0 s0)
                                      <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly empty) set of available Del mutations that would
-- take the given joint out of the type (assumes it's in)
delMutsOf :: PrimMonad m =>
             TypeState (PrimState m) -> Sym -> Sym -> m [Mutation]
delMutsOf (TS _ u0 _ u1) s0 s1 = delMutsOf_ <$> sequence (s0, MV.read u0 s0)
                                            <*> sequence (s1, MV.read u1 s1)

-- | Give the (possibly missing) mutation that would make the given
-- joint member of the type (assumes it's not)
addMutOf :: PrimMonad m =>
            TypeState (PrimState m) -> Sym -> Sym -> m (Maybe Mutation)
addMutOf (TS _ u0 _ u1) s0 s1 = addMutOf_ <$> sequence (s0, MV.read u0 s0)
                                          <*> sequence (s1, MV.read u1 s1)

----------
-- INIT --
----------

-- | Given the number of symbols, a list of all joints and a joint type,
-- return the SymEntries of the left and right unions of the type
init :: PrimMonad m => Int -> [(Sym,Sym)] -> JointType ->
        m (TypeState (PrimState m))
init m allJoints (JT u0 u1) = do
  uLeft  <- MV.replicate m emptyOut -- uLeft
  uRight <- MV.replicate m emptyOut -- uRight
  forM_ s0s $ flip (MV.write uLeft ) emptyIn
  forM_ s1s $ flip (MV.write uRight) emptyIn

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

  return $ TS sz0 uLeft sz1 uRight
  where
    s0s = UT.toList u0 -- left member symbols
    s1s = UT.toList u1 -- right member symbols
    sz0 = UT.size u0
    sz1 = UT.size u1

------------
-- UPDATE --
------------

data SymEntry_ = SymEntry_
  { __isMember :: !Bool -- ^ True iff self is member of the union type
  , __coSymsIn :: !IntSet -- ^ Symbols that have a joint with
                         -- self and member of the co-union
  , __dependents :: !IntSet -- ^ CoSymsIn that have self as only coSymsIn
  , __coSymsOut :: !IntSet } -- ^ Symbols that have a joint with self and
                            -- *not* member of the co-union
  deriving (Show,Eq,Ord)


pushMut :: PrimMonad m => Mutation -> TypeT m ()
pushMut = \case AddLeft s0 -> addLeft s0
                AddRight s1 -> addRight s1
                Add2 s0 s1 -> addLeft s0 >> addRight s1
                DelLeft s0 -> delLeft s0
                DelRight s1 -> delRight s1
                Del2 s0 s1 -> delLeft s0 >> delRight s1
  where
    addLeft s0 = do
      leftSize += 1
      e0@(SymEntry mem coIn deps coOut) <- readLeft s0
      when mem $ err' $ "addLeft: symbol already member: " ++ show s0
      unless (IS.null deps) $
        err' $ "addLeft: out-sym shouldn't have deps: " ++ show (s0,deps)
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyRight $ \e1 -> e1 & coSymsIn  %~ IS.insert s0
                                & coSymsOut %~ IS.delete s0
      writeLeft s0 $ e0 & isMember .~ True

    addRight s1 = do
      rightSize += 1
      e1@(SymEntry mem coIn deps coOut) <- readRight s1
      when mem $ err' $ "addRight: symbol already member: " ++ show s1
      unless (IS.null deps) $
        err' $ "addRight: out-sym shouldn't have deps: " ++ show (s1,deps)
      forM_ (IS.toList coIn ++ IS.toList coOut) $
        modifyLeft $ \e0 -> e0 & coSymsIn  %~ IS.insert s1
                               & coSymsOut %~ IS.delete s1
      writeRight s1 $ e1 & isMember .~ True

    delLeft s0 = do
      leftSize -= 1
      e0@(SymEntry mem coIn deps coOut) <- readLeft s0
      unless mem $ err' $ "delLeft: symbol not member: " ++ show s0
      unless (IS.null deps) $
        err' $ "delLeft: can't del sym with deps: " ++ show (s0,deps)

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

    delRight s1 = do
      rightSize -= 1
      e1@(SymEntry mem coIn deps coOut) <- readRight s1
      unless mem $ err' $ "delRight: symbol not member: " ++ show s1
      unless (IS.null deps) $
        err' $ "delRight: can't del sym with deps: " ++ show (s1,deps)

      forM_ (IS.toList coIn) $ \s0 -> do
        e0 <- readLeft s0
        let e0' = e0 & coSymsIn  %~ IS.delete s1
                     & coSymsOut %~ IS.insert s1
        whenJust (trySingleton $ e0'^.coSymsIn) $ modifyRight $
          dependents %~ IS.insert s0 -- mark s0 as dependent to _
        writeLeft s0 e0'

      forM_ (IS.toList coOut) $
        modifyLeft $ \e0 -> e0 & coSymsIn  %~ IS.delete s1
                               & coSymsOut %~ IS.insert s1

      writeRight s1 $ e1 & isMember .~ False

    err' = err . ("pushMut: " ++)

    trySingleton :: IntSet -> Maybe Sym
    trySingleton is | [s] <- IS.toList is = Just s
                    | otherwise = Nothing

err :: String -> a
err = error . ("TypeState." ++)

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
-- the out-interval is not itself preceded by another
-- in-interval. Returns `Nothing` if the preceding interval is so
-- sandwitched, `Just Nothing` if there is either no preceding joint
-- (begining of the string) or if it not add-able, and `Just Just` if
-- there is such a mutation-interval pair.
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
nextMutCIs str tst ci@(CI _ _ _ i0 s0) = (D.next str i0 >>=) $ \case
  Nothing -> return Nothing -- hit end
  Just (i1,s1) -> (addMutOf tst s0 s1 >>=) $ \case
    Nothing -> return Nothing -- no add-mutation
    Just addMut -> Just . (addMut,) <$> grabOut [ci] (CI s0 i0) 2 i1 s1
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
