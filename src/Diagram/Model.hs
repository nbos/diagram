{-# LANGUAGE LambdaCase, BangPatterns #-}
module Diagram.Model (module Diagram.Model) where

import Control.Monad.Primitive (PrimMonad)

import Data.Word
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as MV

import Streaming
import qualified Streaming.Prelude as S

data Model = Model {
  -- modelTypes :: !Types,
  modelTotalCount :: !Int,
  modelCounts :: !(U.Vector Int)
  -- modelJointCounts :: !(Map (Sym,Sym) Int) -- why?
}

type Sym = Int

-- -- | Construction from bytes
-- fromAtoms :: PrimMonad m => Stream (Of Word8) m r -> m (Model, r)
-- fromAtoms ss = do
--   (ks,(jts,r)) <- countAtoms $
--                   countJointsM $ S.map fromEnum $
--                   S.copy ss
--   let n = U.foldl' (+) 0 ks
--   return (Model n ks jts, r)

-- | Histogram of the 256 bytes in a stream
countAtoms :: PrimMonad m => Stream (Of Word8) m r -> m (U.Vector Int, r)
countAtoms ss = do
  mks <- U.unsafeThaw $ U.replicate 256 0
  r <- S.effects $ S.mapM (MV.modify mks (+1) . fromEnum) ss
  ks <- U.freeze mks
  return (ks, r)

-- | Count the constructable joints in a stream
countJointsM :: Monad m => Stream (Of Int) m r -> m (Map (Sym,Sym) Int, r)
countJointsM = countJointsM_ M.empty

countJointsM_ :: Monad m => Map (Sym,Sym) Int -> Stream (Of Int) m r ->
                            m (Map (Sym,Sym) Int, r)
countJointsM_ m0 ss0 = (S.next ss0 >>=) $ \case
  Left r -> return (m0, r)
  Right (s0,ss0') -> go m0 s0 ss0'
  where
    go !m s0 ss = (S.next ss >>=) $ \case
      Left r -> return (m,r) -- end
      Right (s1,ss') -> (S.next ss' >>=) $ \case
        Left r -> return (m', r) -- last joint
        Right (s2,ss'') | s0 == s1 && s1 == s2 ->
                            countJointsM_ m' $ S.yield s2 >> ss'' -- even
                        | otherwise -> go m' s1 $ S.yield s2 >> ss'' -- odd
        where m' = M.insertWith (+) (s0,s1) 1 m
