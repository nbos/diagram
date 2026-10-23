{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrIntervals.Internal
  (module Diagram.ConstrIntervals.Internal) where

import GHC.Show

import Control.Monad hiding (join)
import Control.Lens hiding (Index,(:>))

import Data.Maybe
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Streaming (Of(..), Stream)
import qualified Streaming.Prelude as S

import Diagram.String
import Diagram.Primitive

import Diagram.JointType (JointType(..))
import qualified Diagram.JointType as JT

import Diagram.ConstrInterval (CI(..))
import qualified Diagram.ConstrInterval as CI

import Diagram.Util

-- A set of construct[ion/ive] intervals, keeping track of the counts of
-- constructed symbols.

-- | Constructive Intervals. Joint concordance\/index. All construction
-- sites in corpus, as intervals, for fast join\/union but slower
-- delete\/subtract (have first to resolve each subtracted interval into
-- its super-interval).
data CIs = CIs
  { _jointType :: !JointType      -- :: (u0, u1)
  , _symCounts :: !(IntMap Count) -- :: s  --> n
  , _byHead    :: !(IntMap CI)    -- :: hd --> (hd, shd, len, tl, stl)
  , _byTail    :: !(IntMap CI) }  -- :: tl --> (hd, shd, len, tl, stl)
  deriving(Eq) -- TODO: joint count?
makeLenses ''CIs

instance Show CIs where
  showsPrec :: Int -> CIs -> ShowS
  showsPrec p cis@(CIs jt ns bhd btl)
    | cis == empty = showString "empty"
    | otherwise = showParen (p >= 11) $
                  showString "CIs {"
                  . showString "_jointType = " . showsPrec 0 jt
                  . showCommaSpace
                  . showString "_symCounts = " . showsPrec 0 ns
                  . showCommaSpace
                  . showString "_byHead = " . showsPrec 0 bhd
                  . showCommaSpace
                  . showString "_byTail = " . showsPrec 0 btl . showString "}"

toList :: CIs -> [CI]
toList = IM.elems . _byHead

-- | Count the number of constructions
jointCount :: CIs -> Int
jointCount (CIs _ ns _ _) | odd sum_ns = error ""
                          | otherwise = sum_ns `div` 2
  where sum_ns = sum ns

------------------
-- CONSTRUCTION --
------------------

-- | Empty set of construction intervals
empty :: CIs
empty = CIs JT.bot e e e
  where e = IM.empty

-- | A singleton set containing a singleton interval
singleton :: (Index,Sym) -> (Index,Sym) -> CIs
singleton is0@(i0,s0) is1@(i1,s1) = CIs jt ns bhd btl
  where jt = JT.singleton s0 s1
        ns = IM.fromList [(s0,1),(s1,1)]
        ci = CI.singleton is0 is1
        bhd = IM.singleton i0 ci
        btl = IM.singleton i1 ci

-- | Construction, for every pair of symbols in the given source string,
-- of the set of continuous construction intervals.
fromStream :: Monad m => Stream (Of (Index,Sym)) m r -> m (Map (Sym,Sym) CIs, r)
fromStream = flip fromStream_ M.empty

-- | Construction given a source string and an accumulator map keyed on
-- joints.
fromStream_ :: Monad m =>
  Stream (Of (Index,Sym)) m r -> Map (Sym,Sym) CIs -> m (Map (Sym,Sym) CIs, r)
fromStream_ ss m = (S.next ss >>=) $ \case
  Left r -> return (m, r)
  Right (is,ss') -> fromStream_0 is ss' m

-- | Construction given a head symbol, the rest of the source string and
-- an accumulator map keyed on joints.
fromStream_0 :: Monad m => (Index,Sym) ->
  Stream (Of (Index,Sym)) m r -> Map (Sym,Sym) CIs -> m (Map (Sym,Sym) CIs, r)
fromStream_0 is0@(i0,s0) ss !m = (S.next ss >>=) $ \case
  Left r -> return (m, r)
  Right (is1@(i1,s1),ss')
    | s0 /= s1 -> fromStream_0 (i1,s1) ss' $
      let ci = CI i0 s0 2 i1 s1
      in flip3 M.insertWith (s0,s1) (singleton is0 is1) m $ \_ ->
        (symCounts %~ IM.insertWith (+) s0 1 . IM.insertWith (+) s1 1)
        . (byHead %~ IM.insertWithKey err' i0 ci)
        . (byTail %~ IM.insertWithKey err' i1 ci)

    | otherwise -> do -- s0 == s1
        is :> ss'' <- S.toList $ S.map fst $ S.span ((s0 ==) . snd) ss'
        let len = length is + 2
            itl = last $ i1:is
            constrlen = (len `div` 2) * 2
            empty' = empty{ _jointType = JT.singleton s0 s0 }
            ci = CI i0 s0 len itl s0

        fromStream_0 (itl,s0) ss'' $ m & at (s0,s0) . non empty' %~
          (symCounts %~ IM.insertWith (+) s0 constrlen)
          . (byHead %~ IM.insertWithKey err' i0 ci)
          . (byTail %~ IM.insertWithKey err' itl ci)

  where
    err' :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err' = err . ("fromStream: collision: " ++) . show .:. (,,)

-- | Delete a CI which is part of set from the set. Symbol counts will
-- be messed up if the CI is not member of the of set. Doesn't modify
-- the joint type.
deleteExisting :: PrimMonad m => Doubly (PrimState m) -> CI -> CIs -> m CIs
deleteExisting dly (CI hd shd 2 tl stl) (CIs jt ns bhd btl) = do
  checkIntervalsIntegrity dly res -- (debug)
  pure res
  where
    res = CIs{ _jointType = jt
             , _symCounts = IM.update (nothingIf (== 0) . (+(-1))) shd $
                            IM.update (nothingIf (== 0) . (+(-1))) stl ns
             , _byHead = IM.delete hd bhd
             , _byTail = IM.delete tl btl }

deleteExisting dly ci@(CI hd _ _ tl _) (CIs jt ns bhd btl) = do
  ndns <- CI.symCounts dly ci
  let res = CIs{ _jointType = jt
               , _symCounts = flip2 IM.differenceWith ns ndns $
                              nothingIf (== 0) .: (-)
               , _byHead = IM.delete hd bhd
               , _byTail = IM.delete tl btl }
  checkIntervalsIntegrity dly res -- (debug)
  return res

-- | Insert a CI which is not present, nor overlapping with any interval
-- (including point contact, or end-to-end-touching) in the
-- set. Invariants break if the condition doesn't hold. Doesn't modify
-- the joint type.
insertDisjoint :: PrimMonad m => Doubly (PrimState m) -> CI -> CIs -> m CIs
insertDisjoint dly ci@(CI hd shd 2 tl stl) (CIs jt ns bhd btl) = do
  checkIntervalsIntegrity dly res -- (debug)
  pure res
  where
    res = CIs{ _jointType = jt
             , _symCounts = IM.insertWith (+) shd 1 $
                            IM.insertWith (+) stl 1 ns
             , _byHead = IM.insert hd ci bhd
             , _byTail = IM.insert tl ci btl }
insertDisjoint dly ci@(CI hd _ _ tl _) (CIs jt ns bhd btl) = do
  dns <- CI.symCounts dly ci
  let res = CIs{ _jointType = jt
               , _symCounts = IM.unionWith (+) dns ns
               , _byHead = IM.insert hd ci bhd
               , _byTail = IM.insert tl ci btl }
  checkIntervalsIntegrity dly res -- (debug)
  return res

err :: String -> a
err = error . ("ConstrIntervals." ++)

-----------
-- DEBUG --
-----------

-- PURE --
-- | Pure version of checkIntegrityM, doesn't check against a string.
checkIntegrity :: CIs -> a -> a
checkIntegrity cis@(CIs _ ns bhd btl)
  | odd (sum ns) = err' $ "sum of counts is odd: " ++ show ns
  | bhdCIs /= btlCIs =
      err' $ "byHead and byTail don't contain the same CI's: " ++ show (bhd,btl)
  | not (and (IM.intersectionWith (<=) minCounts ns)) =
      err' $ "recorded symbol counts impossibly low: "
      ++ show ((cisL,ns),minCounts)
  | otherwise = checkIntervalsAgainstType cis
  where
    err' = err . ("checkIntegrity: " ++)
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs
    minCounts = L.foldl' (\m s -> IM.insertWith (+) s (1::Int) m) IM.empty $
                foldr (\(CI _ shd len _ stl) l ->
                         if even len then shd:stl:l else shd:l) [] cisL

checkIntegrityId :: CIs -> CIs
checkIntegrityId cis = checkIntegrity cis cis

checkIntegrity_ :: Monad m => CIs -> m ()
checkIntegrity_ cis = checkIntegrity cis $ return ()

checkIntervalsAgainstType :: CIs -> a -> a
checkIntervalsAgainstType (CIs jt _ bhd _) = foldr (.) id $
  (<$> IM.elems bhd) $ \ci@(CI _ shd _ _ stl) ->
                 if JT.member jt shd stl then id
                 else err $ "checkIntervalsAgainstType: edges of interval "
                      ++ show ci ++ " don't fit in type " ++ show jt

-- MONADIC --
checkIntegrityM :: PrimMonad m => Doubly (PrimState m) -> CIs -> m ()
checkIntegrityM dly cis = checkIntervalsIntegrity dly cis
                          >> checkJointsAgainstType dly cis
-- where --

-- | Don't check against type, so this can be used before/after manual
-- editing of the interval set before/after updating the type
checkIntervalsIntegrity :: PrimMonad m => Doubly (PrimState m) -> CIs -> m ()
checkIntervalsIntegrity dly (CIs _ ns bhd btl)
  | odd (sum ns) = err' $ "sum of counts is odd: " ++ show ns
  | bhdCIs /= btlCIs =
      err' $ "byHead and byTail don't contain the same CI's: " ++ show (bhd,btl)
  | otherwise = do
      ns_verif <- IM.unionsWith (+) <$> mapM (CI.symCounts dly) cisL
      when (ns /= ns_verif) $
        err' $ "sym counts don't match extension's: " ++ show (ns, ns_verif)
  where
    err' = err . ("checkIntegrity: " ++)
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs

-- | Check against the type
checkJointsAgainstType :: PrimMonad m => Doubly (PrimState m) -> CIs -> m ()
checkJointsAgainstType dly (CIs jt _ bhd _) = do
  (s0s, s1s) <- unzip . concat <$> mapM (CI.jointExtension dly) (IM.elems bhd)
  let jt_verif = JT.fromLists s0s s1s
  when (jt /= jt_verif) $
    err $ "checkJointsAgainstType: type doesn't correspond to member joints:"
    ++ "\nis:\n" ++ show jt ++ "\nshould be:\n" ++ show jt_verif ++ "\n"

-- BOOLEAN --
-- | Errorless version of @checkIntegrity@
valid :: PrimMonad m => Doubly (PrimState m) -> CIs -> m Bool
valid dly (CIs jt ns bhd btl) = do
  (s0s, s1s) <- unzip . concat <$> mapM (CI.jointExtension dly) cisL
  let jt_verif = JT.fromLists s0s s1s
  ns_verif <- unions <$> mapM (CI.symCounts dly) cisL
  return $ even (sum ns)
    && bhdCIs == btlCIs && jt == jt_verif && ns == ns_verif
  where
    bhdCIs = L.sort $ IM.elems bhd
    btlCIs = L.sort $ IM.elems btl
    cisL = bhdCIs
    unions = fromMaybe IM.empty . foldTree (IM.unionWith (+))
