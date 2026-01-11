{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Diagram.TrainJointType (module Diagram.TrainJointType) where

import Control.Monad.Random

import Data.Function
import Data.Tuple.Extra
import qualified Data.List.Extra as L
import Data.IntSet (IntSet)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M

import Streaming hiding (first, second)
import qualified Streaming.Prelude as S

import Diagram.Model (Sym)
import Diagram.Joints (Joints)
import qualified Diagram.Joints as Jts
import qualified Diagram.UnionType as U
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import qualified Diagram.Random as R

-- | A joint of unions with the joints it covers (its extension). The
-- joint type is not necessarily the least upper bound of its extension
-- (a refinement may have taken some joints away)
data TrainJointType = TJT {
  joints    :: !Joints,   -- :: Map (Sym,Sym) (Int,IntSet)
  jointType :: !JointType -- :: ((Int,IntSet), (Int,IntSet))
} deriving (Eq,Show)

fromJoints :: Joints -> TrainJointType
fromJoints jts = TJT jts (JT.fromJoints jts)

-- | Check the LUB property
isLUB :: TrainJointType -> Bool
isLUB (TJT jts jt) = jt == JT.fromJoints jts

join :: TrainJointType -> TrainJointType -> TrainJointType
join (TJT jts0 jt0) (TJT jts1 jt1) = TJT jts jt
  where jts = Jts.union jts0 jts1
        jt = JT.join jt0 jt1

-- | Generate a random refinement that is the least-upper-bound (LUB) of
-- the joints it covers. This is different than a random refinement of
-- each union type, because of the joints involved. Tries to be fancy
-- and shuffle so whatever bias in our enforcement of the invariants of
-- the LUB property is distributed across the lattice. The given joint
-- type is returned (fst) with the joints covered by the refinement
-- (snd) removed.
genRefinement :: forall m. MonadRandom m => TrainJointType ->
                 m (TrainJointType, TrainJointType)
genRefinement (TJT jts jt) = do
  let jtsByFst = L.groupBy ((==) `on` (fst . fst)) $
                 M.toAscList jts
  (notSel,sel) <- R.split jtsByFst
  let rs0s = fst . fst . head <$> sel
      ru0 = U.fromDistinctAscList rs0s

  rjts :> jts' :> rs1s <-
    S.fold M.union M.empty id
    . S.fold M.union (M.fromDistinctAscList $ concat notSel) id
    . S.unzip
    . go IM.empty =<< R.shuffle (length sel) sel

  let ru1 = U.fromSet rs1s
      rjt = JT ru0 ru1
  return (TJT jts' jt, TJT rjts rjt)

  where
    go :: IntMap () -> [[((Sym,Sym),(Int,IntSet))]] ->
          Stream (Of (Joints,Joints)) m IntSet
    go s1s [] = return $ IM.keysSet s1s
    go s1s (jt0s:rest) = do
      (notSel_,sel_) <- lift $ R.split toSampleL
      (notSel,sel) <- if null sel_ && IM.disjoint s1s toSample
        then do -- LUB property broken, resample ensuring >= 1 sel
        let len = length toSampleL
        i <- lift $ getRandomR (0,len-1)
        let (lt,(s1,gt)) = (head &&& tail) <$> splitAt i toSampleL
        (notSelLT,selLT) <- lift $ R.split lt
        (notSelGT,selGT) <- lift $ R.split gt
        return (notSelLT ++ notSelGT, selLT ++ s1:selGT)
        else return (notSel_,sel_) -- LUB not broken

      S.yield ( M.fromDistinctAscList notSel
              , M.fromDistinctAscList sel )
      let sel1s = IM.fromDistinctAscList $ (,()) . snd . fst <$> sel
      go (s1s `IM.union` sel1s) rest

      where
        s0 = fst $ fst $ head jt0s
        jt0sBySnd = IM.fromDistinctAscList $ first snd <$> jt0s
        toSample = jt0sBySnd IM.\\ s1s
        toSampleL = first (s0,) <$> IM.toList toSample
