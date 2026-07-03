{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE TupleSections, LambdaCase, TypeApplications, TypeOperators, BangPatterns #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.JointType.Random (module Diagram.JointType.Random) where

import Control.Monad as Monad
import Control.Lens hiding (both,last1,Index,(:>))
import Control.Monad.State.Strict ( StateT
                                  , MonadState(get)
                                  , evalStateT )
import Control.Monad.Random (MonadRandom(getRandomR, getRandom))
import Control.Monad.IO.Class (MonadIO(..))

import Data.Maybe
import Data.Tuple.Extra
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Diagram.Primitive

import Diagram.String
import Diagram.Joints (Joints,Joints2S(J2S))
import qualified Diagram.Joints as Jts
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT

import Diagram.Util

err :: String -> a
err = error . ("JointType.Random." ++)

-- ----------------------- --
-- -- RANDOM GENERATION -- --
-- ----------------------- --

-- | O(n^2) Generate all refinements given joints indexed both ways
-- starting with the empty refinement, ending with the same as input
--
-- >>> import qualified Diagram.Joints as Jts
-- >>> enum $ Jts.sized $ Jts.doubleIndex 256 $ Jts.fromList [1,2,3,2]
-- [ fromLists ([],[])
-- , fromLists ([3],[2])
-- , fromLists ([2],[3])
-- , fromLists ([2,3],[2,3])
-- , fromLists ([1],[2])
-- , fromLists ([1,3],[2])
-- , fromLists ([1,2],[2,3])
-- , fromLists ([1,2,3],[2,3]) ]
enum :: forall a. Joints2S a -> [JointType]
enum (J2S byFst0 bySnd0) = concatMap givenU0 u0s
  where -- (enumerate u0 powerset, then hitting set enum)
    u0s :: [[(Sym, Map Sym a)]]
    u0s = combs $ M.toAscList byFst0 -- deconstruct, select

    givenU0 :: [(Sym, Map Sym a)] -> [JointType]
    givenU0 s0ns0ss = JT u0 . UT.fromDistinctAscList
                      <$> go byFst (M.toAscList bySnd)
      where
        byFst = M.fromAscList s0ns0ss -- reconstruct
        bySnd = fmap (`M.intersection` byFst) $ -- restrict to s0s
                bySnd0 `M.intersection` s1s -- restrict to s1s
        (u0,s1s) = UT.fromDistinctAscList *** M.unions $ unzip s0ns0ss

    go :: Map Sym (Map Sym a) -> [(Sym, Map Sym a)] -> [[Sym]]
    go _ [] = [[]]
    go byFst ((s1,s0s):bySnd)
      | malformed = error "enumRefinements: malformed"
      | notFree = sel
      | otherwise = notSel ++ sel
      where
        ns1s = byFst `M.intersection` s0s
        malformed = any (s1 `M.notMember`) ns1s
        notFree = any ((== 1) . M.size) ns1s
        sel = (s1:) <$> go byFst bySnd -- leave s1 in

        -- remove s1 if not selected
        ns1s' = M.delete s1 <$> ns1s
        byFst' = ns1s' `M.union` byFst
        notSel = go byFst' bySnd

-- | O(n^2) Generate all combinations
combs :: [a] -> [[a]]
combs [] = [[]]
combs (a:as) = ass ++ fmap (a:) ass
  where ass = combs as

-- | State record to track the two maps and two sets
data GenerationState a = GenerationState {
  -- NOTE: Map Int instead of IntMap because we want O(1) size
  -- and O(log n) elemAt.
  _jtsByFst :: !(Map Sym ([(Sym,a)], Map Sym a)),
  _jtsBySnd :: !(Map Sym ([(Sym,a)], Map Sym a)),
  _fstUnion :: !IntSet,
  _sndUnion :: !IntSet,
  _refJoints :: !(Map (Sym,Sym) a)
}
makeLenses ''GenerationState

-- | Generate a random refinement, given a set of joints indexed both
-- ways. Assumes each map maps pairs of symbols to the same values,
-- i.e. if s0 -> s1 -> a01 then s1 -> s0 -> a01, otherwise the returned
-- map of joints will have unpredicatble values.
genRandom :: (MonadRandom m, PrimMonad m) =>
             Joints2S a -> m (JointType, Joints a)
genRandom = genRandomWith 0.5

-- | Generate a random refinement, given a sampling probability
genRandomWith :: forall m a. (MonadRandom m, PrimMonad m) =>
                 Double -> Joints2S a -> m (JointType, Joints a)
genRandomWith r (J2S byFst0 bySnd0) =
  evalStateT go $ GenerationState
  (([],) <$> byFst0)
  (([],) <$> bySnd0) IS.empty IS.empty M.empty
  where
    go :: StateT (GenerationState a) m (JointType, Map (Sym,Sym) a)
    go = get >>= \case
      (GenerationState byFst bySnd u0 u1 ref)
        | remaining == 0 -> return ( JT (UT.fromSet u0) (UT.fromSet u1)
                                   , ref ) -- end
        | otherwise -> do
            i <- getRandomR (0, remaining-1) -- select a symbol
            f <- getRandom @_ @Double -- include/exclude it in the ref
            let b = f <= r
            if i < len0 then goElimFst b (fst $ M.elemAt i byFst)
              else goElimSnd b (fst $ M.elemAt (i - len0) bySnd)
            go -- rec
        where
          len0 = M.size byFst -- O(1)
          len1 = M.size bySnd -- O(1)
          remaining = len0 + len1

    -- | Map.deleteFind
    deleteFind :: Ord k => k -> Map k b -> (b, Map k b)
    deleteFind = first fromJust
                 .: M.updateLookupWithKey (\_ _ -> Nothing)

    -- | Eliminate a symbol and enforce invariants whether it has be
    -- `sel`ected or not
    goElimFst :: Bool -> Int -> StateT (GenerationState a) m ()
    goElimFst sel0 s0 = do
      (staged0, jt0s) <- jtsByFst %%= deleteFind s0 -- remove from avail.
      when sel0 $ do
        fstUnion %= IS.insert s0 -- add to JointType
        forM_ staged0 $ \(s1,a01) -> -- add staged Joints
          refJoints %= M.insert (s0,s1) a01

      -- unlink from neighbors
      deleted1 <- forM (M.keys jt0s) $ \s1 -> do
        jtsBySnd . at s1 %%= \case
          Nothing -> error "impossible"
          Just (staged1, jt1s) -- -> (deleted, inserted)
            | null staged1' && M.null jt1s' -> (Just s1, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged1', jt1s')) -- update
            where
              staged1' | sel0 = (s0,a01):staged1 -- stage s0 on s1 if selected
                       | otherwise = staged1
              (a01,jt1s') = deleteFind s0 jt1s

      -- enforce invariant if necessary
      when (sel0 && null staged0) $ do
        i <- getRandomR (0, M.size jt0s - 1) -- select a symbol
        let (s1, a01) = M.elemAt i jt0s
        if Just s1 `notElem` deleted1 then goElimSnd True s1 -- rec
          else do sndUnion %= IS.insert s1
                  refJoints %= M.insert (s0,s1) a01 -- null staged1

    -- | Symmetric with above, could probably be factored into one, but
    -- ehhh
    goElimSnd :: Bool -> Int -> StateT (GenerationState a) m ()
    goElimSnd sel1 s1 = do
      (staged1, jt1s) <- jtsBySnd %%= deleteFind s1 -- remove from avail.
      when sel1 $ do
        sndUnion %= IS.insert s1 -- add to JointType
        forM_ staged1 $ \(s0,a01) -> -- add staged Joints
          refJoints %= M.insert (s0,s1) a01

      -- unlink from neighbors
      deleted0 <- forM (M.keys jt1s) $ \s0 -> do
        jtsByFst . at s0 %%= \case
          Nothing -> error "impossible"
          Just (staged0, jt0s) -- -> (deleted, inserted)
            | null staged0' && M.null jt0s' -> (Just s0, Nothing) -- delete
            | otherwise -> (Nothing, Just (staged0', jt0s')) -- update
            where
              staged0' | sel1 = (s1,a01):staged0 -- stage s1 on s0 if selected
                       | otherwise = staged0
              (a01,jt0s') = deleteFind s1 jt0s

      -- enforce invariant if necessary
      when (sel1 && null staged1) $ do
        i <- getRandomR (0, M.size jt1s - 1) -- select a symbol
        let (s0, a01) = M.elemAt i jt1s
        if Just s0 `notElem` deleted0 then goElimFst True s0 -- rec
          else do fstUnion %= IS.insert s0
                  refJoints %= M.insert (s0,s1) a01 -- null staged0

--------------
-- IO STATS --
--------------

printInfo :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
             (JointType, Map (Sym,Sym) b) -> m ()
printInfo (jt,jts) (rjt,rjts) = liftIO $ putStrLn $
  "generated refinement type with size "
  ++ show (JT.dims rjt)
  ++ " from "  ++ show (JT.dims jt)
  ++ " covering " ++ show (Jts.size rjts)
  ++ " joints out of " ++ show (Jts.size jts)
  ++ " ("  ++ show
  (round @_ @Int $ 100.0 * fromIntegral (Jts.size rjts)
    / fromIntegral @_ @Double (Jts.size jts))
  ++ "%)"

printLUB :: MonadIO m => JointType -> Map (Sym,Sym) a -> m ()
printLUB jt jts = liftIO $ do
  putStr "refinement is "
  if jt == JT.fromJoints jts
    then putStrLn $ inGreen "LUB" ++ " of its joints"
    else do putStrLn $ inRed "not LUB" ++ " of its joints"
            putStrLn $ "rtjt: " ++ show (jt, void jts)
            error "LUB error"

printSubtyping :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                  (JointType, Map (Sym,Sym) b) -> m ()
printSubtyping (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "refinement is "
  if rjt `JT.leq` jt
    then putStrLn $ inGreen "subtype" ++ " of its parent"
    else do putStrLn $ inRed "not subtype" ++ " of its parent"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "subtype error"

printConservation :: MonadIO m => (JointType, Map (Sym,Sym) a) ->
                     (JointType, Map (Sym,Sym) a) -> m ()
printConservation (jt,jts) (rjt,rjts) = liftIO $ do
  let jts' = jts M.\\ rjts
  putStr "split " -- TODO: check disjointness too?
  if void jts == (void rjts `M.union` void jts')
    then putStrLn $ inGreen "preserves" ++ " all joints"
    else do putStrLn $ inRed "does not preserve" ++ " all joints"
            putStrLn $ "tjt: " ++ show (jt, void jts)
              ++ "\ntjt': " ++ show (jt, void jts')
              ++ "\nrtjt: " ++ show (rjt, void rjts)
            error "joints split error"

printMembership :: MonadIO m => Map (Sym,Sym) a -> (JointType, Map (Sym,Sym) a) -> m ()
printMembership jts (rjt,rjts) = liftIO $ do
  let rjtsVerif = M.filterWithKey (\k _ -> k `JT.member` rjt) jts
  putStr "returned joints "
  if M.keys rjts == M.keys rjtsVerif
    then putStrLn $ inGreen "match" ++ " joints covered by the refinement"
    else do putStrLn $ inRed "don't match" ++ " joints covered by the refinement"
            putStrLn $ "rtjt: " ++ show (M.keys rjts)
              ++ "\nrjts: " ++ show (M.keys rjts)
              ++ "\nrjtsVerif: " ++ show (M.keys rjtsVerif)
            error "joints coverage error"

inRed :: String -> String
inRed s = "\ESC[31mError:" ++ s ++ "\ESC[0m"

inGreen :: String -> String
inGreen s = "\ESC[32m" ++ s ++ "\ESC[0m"
