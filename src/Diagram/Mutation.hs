{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections, BangPatterns #-}
module Diagram.Mutation (module Diagram.Mutation) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U

import qualified Streaming.Prelude as S

import Diagram.Primitive
import Diagram.Information
import Diagram.Doubly (Doubly,Index)
import qualified Diagram.Doubly as D

import Diagram.UnionType (Sym)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT

--------------
-- MUTATION --
--------------

-- data Mutation =
--   AddLeft  !Sym !(IntMap Int)
--   | AddRight !Sym !(IntMap Int)
--   | Add2     !Sym !Sym !Int
--   | DelLeft  !Sym !(IntMap Int)
--   | DelRight !Sym !(IntMap Int)
--   | Del2     !Sym !Sym !Int
--   deriving(Show,Eq)

data NumAffected = One | Two
  deriving (Show,Eq,Ord)

type family NewJointCounts (k :: NumAffected) where
  NewJointCounts One = IntMap Int -- ins opposite side
  NewJointCounts Two = Int -- n01

data Mutation (k :: NumAffected) where
  AddLeft  :: !Sym         -> Mutation One
  AddRight :: !Sym         -> Mutation One
  Add2     :: !Sym -> !Sym -> Mutation Two
  DelLeft  :: !Sym         -> Mutation One
  DelRight :: !Sym         -> Mutation One
  Del2     :: !Sym -> !Sym -> Mutation Two

deriving instance Show (Mutation k)
deriving instance Eq (Mutation k)
deriving instance Ord (Mutation k)

---------------------------------
-- SOME MUTATION (EXISTENTIAL) --
---------------------------------

data SomeMutation where
  Mut :: !(Mutation k) -> SomeMutation

instance Show SomeMutation where
  show :: SomeMutation -> String
  show (Mut mut) = case mut of
    AddLeft  s   -> "AddLeft "  ++ show s
    AddRight s   -> "AddRight " ++ show s
    Add2     a b -> "Add2 "     ++ show a ++ " " ++ show b
    DelLeft  s   -> "DelLeft "  ++ show s
    DelRight s   -> "DelRight " ++ show s
    Del2     a b -> "Del2 "     ++ show a ++ " " ++ show b

instance Eq SomeMutation where
  (==) :: SomeMutation -> SomeMutation -> Bool
  Mut m1 == Mut m2 = case (m1, m2) of
    (AddLeft  s1,    AddLeft  s2)    -> s1 == s2
    (AddRight s1,    AddRight s2)    -> s1 == s2
    (Add2     a1 b1, Add2     a2 b2) -> a1 == a2 && b1 == b2
    (DelLeft  s1,    DelLeft  s2)    -> s1 == s2
    (DelRight s1,    DelRight s2)    -> s1 == s2
    (Del2     a1 b1, Del2     a2 b2) -> a1 == a2 && b1 == b2
    _else                            -> False

instance Ord SomeMutation where
  compare :: SomeMutation -> SomeMutation -> Ordering
  compare (Mut m1) (Mut m2) = case (m1, m2) of
    (AddLeft  s1,    AddLeft  s2)    -> compare s1 s2
    (AddRight s1,    AddRight s2)    -> compare s1 s2
    (Add2     a1 b1, Add2     a2 b2) -> compare a1 a2 <> compare b1 b2
    (DelLeft  s1,    DelLeft  s2)    -> compare s1 s2
    (DelRight s1,    DelRight s2)    -> compare s1 s2
    (Del2     a1 b1, Del2     a2 b2) -> compare a1 a2 <> compare b1 b2
    _else                            -> compare (mutTag m1) (mutTag m2)
    where
      mutTag :: Mutation k -> Int
      mutTag AddLeft{}  = 0
      mutTag AddRight{} = 1
      mutTag Add2{}     = 2
      mutTag DelLeft{}  = 3
      mutTag DelRight{} = 4
      mutTag Del2{}     = 5

-- | O(N log(m)) Reads the whole string compiling differences in counts
-- resulting from introduced mutations breaking old constructions
-- through higher precedence
breakingDeltaByMut :: forall m. PrimMonad m => JointType ->
                      Doubly U.MVector (PrimState m) Sym ->
                      m (Map SomeMutation (IntMap Int))
breakingDeltaByMut jt str =
  S.fold_ (flip $ uncurry $ M.insertWith (IM.unionWith (+))) M.empty id $
  S.mapM mkEntry $ JT.runs jt $ D.streamWithKey str
  where
    mkEntry :: (Index, NonEmpty (Sym, Sym)) -> m (SomeMutation, IntMap Int)
    mkEntry (i,ps) = do
      s0 <- D.read str =<< D.prevKey str i
      let (mut, _, s2) = breakingOf jt s0 ps
          im = case compare s0 s2 of
            LT -> IM.fromDistinctAscList [(s0,-1),(s2,1)]
            EQ -> IM.empty -- cancel out
            GT -> IM.fromDistinctAscList [(s2,1),(s0,-1)]
      return (mut, im)

-- | For a joint type and a non-empty sequence of consecutive
-- construction sites (s1,s2):rest, preceded by a non-constructive
-- symbol s0, returns the mutation to that type that would break the
-- sequence, the length of the new run, and the trailing symbol that
-- would be left unconstructed, i.e. whose count would have to be
-- incremented by 1 (while the count of s0 would be correspondingly
-- decremented by 1).
breakingOf :: JointType -> Sym -> NonEmpty (Sym,Sym) -> (SomeMutation, Int, Sym)
breakingOf (JT u0 u1) s0 ((s1,s2):|rest) = uncurry (mut,,) $ go 1 s2 rest
  where
    go !k s2' [] = (k,s2')
    go !k s2' ((s3,s4):rest')
      | (s2',s3) `JT.member` jt' = go (k+1) s4 rest' -- breaking propagates
      | otherwise = (k,s2') -- breaking stops, leaving s2' unconstructed

    (mut, jt')
      | s0 `UT.member` u0 = -- assert $ not (s1 `UT.member` u1)
          (Mut (AddRight s1), JT u0 u1')
      | s1 `UT.member` u1 = (Mut (AddLeft s0), JT u0' u1)
      | otherwise = (Mut (Add2 s0 s1), JT u0' u1')
      where
        u0' = UT.insertMissing s0 u0
        u1' = UT.insertMissing s1 u1

--------------------------------
-- MUTATION WITH COUNTS DELTA --
--------------------------------

data MutationDelta where
  MutD :: !(Mutation k) -> !(NewJointCounts k) -> MutationDelta

instance Show MutationDelta where
  show :: MutationDelta -> String
  show (MutD mut jtns) = case mut of
    AddLeft  s   -> "AddLeft "  ++ show s ++ " " ++ show jtns
    AddRight s   -> "AddRight " ++ show s ++ " " ++ show jtns
    Add2     a b -> "Add2 "     ++ show a ++ " " ++ show b ++ " " ++ show jtns
    DelLeft  s   -> "DelLeft "  ++ show s ++ " " ++ show jtns
    DelRight s   -> "DelRight " ++ show s ++ " " ++ show jtns
    Del2     a b -> "Del2 "     ++ show a ++ " " ++ show b ++ " " ++ show jtns

----------
-- EVAL --
----------

-- | MutDuate the loss incurred by the application of a mutation
evalMutation :: Int -> Int -> U.Vector Int -> IntMap Int -> Int -> Int ->
                MutationDelta -> Double
evalMutation m bigN ns ns' nm vm = go
  where
    deltaDelta_ :: Int -> [(Int,Int)] -> Int -> Double
    deltaDelta_ nm' dns vm' = deltaDelta m bigN (nm,nm') dns (vm,vm')

    go :: MutationDelta -> Double
    go (MutD (AddLeft s0) jtns) = goAdd1 s0 jtns
    go (MutD (AddRight s1) jtns) = goAdd1 s1 jtns
    go (MutD (Add2 s0 s1) n01) = deltaDelta_ nm' dns (vm+2)
      where nm' = nm + n01
            n0' = IM.findWithDefault (ns U.! s0) s0 ns'
            n1' = IM.findWithDefault (ns U.! s1) s1 ns'
            dns | s0 == s1  = [(n0', n0'-2*n01)]
                | otherwise = [(n0', n0'-n01)
                              ,(n1', n1'-n01)]

    go (MutD (DelLeft s0) jtns) = goDel1 s0 jtns
    go (MutD (DelRight s1) jtns) = goDel1 s1 jtns
    go (MutD (Del2 s0 s1) n01) = deltaDelta_ nm' dns (vm-2)
      where
        nm' = nm - n01
        n0 = ns' IM.! s0
        n1 = ns' IM.! s1
        dns | s0 == s1  = [(n0, n0+2*n01)]
            | otherwise = [(n0, n0+n01)
                          ,(n1, n1+n01)]

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goAdd1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm + dnm
        ns'' = IM.insertWith (\_ n0' -> n0') s0 (ns U.! s0) ns'
               -- ^ ensures s0 is in ns' before intersection operation
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'-adn)) ns'' adns
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns' because
              -- each key of jtns is a staged symbol

    -- | Factored; s0 can be either left or right, doesn't make a
    -- difference
    goDel1 s0 jtns = deltaDelta_ nm' (IM.elems dns) (vm+1)
      where
        dnm = sum jtns -- INFO: this could be bookkept
        nm' = nm - dnm
        adns = IM.insertWith (+) s0 dnm jtns -- absolute diffs
        dns = IM.intersectionWith (\n' adn -> (n', n'+adn)) ns' adns
              -- IM.keySet jtns `IS.isSubsetOf` IM.keySet ns'

-- | Computes the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after), and joint type variety (before,after)
deltaDelta :: Int -> Int -> (Int,Int) -> [(Int,Int)] -> (Int,Int) -> Double
deltaDelta m bigN (nm,nm') dns (vm,vm') =
  nDeltaDelta + sDeltaDelta + rDeltaDelta
  where
    nDeltaDelta = logFactNpmmnm' - logFactNpmmnm
    logFactNpmmnm = logFact $ mpN - nm
    logFactNpmmnm' = logFact $ mpN - nm'
    mpN = m + bigN -- m + N

    sDeltaDelta = sDDltm + logFact nm - logFact nm'
    sDDltm = sum $ (<$> dns) $ \(ni',ni'') ->
      logFact ni' - logFact ni''
      -- (logFact ni (old symbol count) cancel out)

    rDeltaDelta = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral
