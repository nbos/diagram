{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE DataKinds, GADTs, TypeFamilies, StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections, BangPatterns #-}
module Diagram.Mutation (module Diagram.Mutation) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.Vector.Unboxed as U

import Diagram.Information

import Diagram.UnionType (Sym)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT

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

data EvalMutation where
  EMut :: !(Mutation k) -> !(NewJointCounts k) -> EvalMutation

instance Show EvalMutation where
  show :: EvalMutation -> String
  show (EMut mut jtns) = case mut of
    AddLeft  s   -> "AddLeft "  ++ show s ++ " " ++ show jtns
    AddRight s   -> "AddRight " ++ show s ++ " " ++ show jtns
    Add2     a b -> "Add2 "     ++ show a ++ " " ++ show b ++ " " ++ show jtns
    DelLeft  s   -> "DelLeft "  ++ show s ++ " " ++ show jtns
    DelRight s   -> "DelRight " ++ show s ++ " " ++ show jtns
    Del2     a b -> "Del2 "     ++ show a ++ " " ++ show b ++ " " ++ show jtns

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

----------
-- EVAL --
----------

-- | Evaluate the loss incurred by the application of a mutation
evalMutation :: Int -> Int -> U.Vector Int -> IntMap Int -> Int -> Int ->
                EvalMutation -> Double
evalMutation m bigN ns ns' nm vm = go
  where
    deltaDelta_ :: Int -> [(Int,Int)] -> Int -> Double
    deltaDelta_ nm' dns vm' = deltaDelta m bigN (nm,nm') dns (vm,vm')

    go :: EvalMutation -> Double
    go (EMut (AddLeft s0) jtns) = goAdd1 s0 jtns
    go (EMut (AddRight s1) jtns) = goAdd1 s1 jtns
    go (EMut (Add2 s0 s1) n01) = deltaDelta_ nm' dns (vm+2)
      where nm' = nm + n01
            n0' = IM.findWithDefault (ns U.! s0) s0 ns'
            n1' = IM.findWithDefault (ns U.! s1) s1 ns'
            dns | s0 == s1  = [(n0', n0'-2*n01)]
                | otherwise = [(n0', n0'-n01)
                              ,(n1', n1'-n01)]

    go (EMut (DelLeft s0) jtns) = goDel1 s0 jtns
    go (EMut (DelRight s1) jtns) = goDel1 s1 jtns
    go (EMut (Del2 s0 s1) n01) = deltaDelta_ nm' dns (vm-2)
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
