{-# LANGUAGE LambdaCase, BangPatterns, TupleSections #-}
module Diagram.Model (module Diagram.Model) where

import Control.Monad
import Control.Monad.Primitive (PrimMonad)

import Data.Word
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Streaming
import qualified Streaming.Prelude as S

import Diagram.UnionType (Sym)
import qualified Diagram.UnionType as UT
import Diagram.JointType (JointType(JT))
import qualified Diagram.JointType as JT
import Diagram.Dynamic (BoxedVec, UnboxedVec)
import qualified Diagram.Dynamic as Dyn
import Diagram.Information

import Diagram.Util

data Model m = Model {
  types :: !(BoxedVec m JointType), -- :: [m - 256]JointType
  stringLen :: !Int,
  nCounts :: !(UnboxedVec m Int), -- [m]Int
  kCounts :: !(UnboxedVec m Int)  -- [m - 256]Int
}

numSymbols :: Model m -> Int
numSymbols = (256+) . Dyn.length . types

------------------
-- CONSTRUCTION --
------------------

-- | Construction from bytes
emptyFromAtoms :: PrimMonad m => Stream (Of Word8) m r -> m (Model m, r)
emptyFromAtoms ss = do
  ts <- Dyn.new
  (nv,r) <- countAtoms ss
  let n = U.foldl' (+) 0 nv
  ns <- Dyn.thaw nv
  ks <- Dyn.new
  return (Model ts n ns ks, r)

-- | Histogram of the 256 bytes in a stream
countAtoms :: PrimMonad m => Stream (Of Word8) m r -> m (U.Vector Int, r)
countAtoms ss = do
  mks <- U.unsafeThaw $ U.replicate 256 0
  r <- S.effects $ S.mapM (MV.modify mks (+1) . fromEnum) ss
  ks <- U.freeze mks
  return (ks, r)

-- | Count the constructable joints in a stream
-- TODO: move to another module
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

-----------------
-- INFORMATION --
-----------------

codeLen :: PrimMonad m => Model m -> m Int
codeLen mdl = (ml + tl + bl + nl +) <$> liftA2 (+) sl rl
  where ml = mLen mdl
        tl = tsLen mdl
        bl = bigNLen mdl
        nl = nsLen mdl
        sl = ssLen mdl
        rl = rsLen mdl

infoLoss :: PrimMonad m => Model m -> Int -> JointType ->
            Map (Sym,Sym) Int -> m Double
infoLoss mdl k jt jts = do (ml + tl + bl + nl + rl +) <$> sl
  where ml = fromIntegral $ mDelta mdl
        tl = fromIntegral $ tsDelta mdl
        bl = fromIntegral $ bigNDelta mdl k
        nl = nsInfoDelta mdl k
        sl = ssInfoDelta mdl k jts
        rl = rsInfoDelta k jt

-- m :: numSymbols

mLen_ :: Int -> Int
mLen_ = eliasCodeLen . (+(-256))

mLen :: Model m -> Int
mLen = mLen_ . numSymbols

mInfo :: Model m -> Double
mInfo = fromIntegral . mLen

mDelta_ :: Int -> Int
mDelta_ m = mLen_ (m+1) - mLen_ m

mDelta :: Model m -> Int
mDelta = mDelta_ . numSymbols

-- ts :: Types

tsLen_ :: Int -> Int
tsLen_ m = m*m - m - 65280

tsLen :: Model m -> Int
tsLen = tsLen_ . numSymbols

tsInfo :: Model m -> Double
tsInfo = fromIntegral . tsLen

tsDelta_ :: Int -> Int
tsDelta_ m = tsLen_ (m+1) - tsLen_ m

tsDelta :: Model m -> Int
tsDelta = tsDelta_ . numSymbols

-- N :: Total count

bigNLen_ :: Int -> Int
bigNLen_ = eliasCodeLen

bigNLen :: Model m -> Int
bigNLen = bigNLen_ . stringLen

bigNInfo :: Model m -> Double
bigNInfo = fromIntegral . bigNLen

bigNDelta_ :: Int -> Int -> Int
bigNDelta_ bigN k = bigNLen_ (bigN - k) - bigNLen_ bigN

-- | Given model and introduced count `k`
bigNDelta :: Model m -> Int -> Int
bigNDelta = bigNDelta_ . stringLen

-- ns :: Counts

nsLen :: Model m -> Int
nsLen = ceiling . nsInfo

nsInfo_ :: Int -> Int -> Double
nsInfo_ m bigN = log2e * ( iLogFactorial (bigN + m - 1)
                           - iLogFactorial (m - 1)
                           - iLogFactorial bigN )

nsInfo :: Model m -> Double
nsInfo mdl = nsInfo_ (numSymbols mdl) (stringLen mdl)

nsInfoDelta_ :: Int -> Int -> Int -> Double
nsInfoDelta_ m bigN k = log2e * ( iLogFactorial (bigN + m - k)
                                  + iLogFactorial bigN
                                  - iLogFactorial (bigN + m - 1)
                                  - log (fromIntegral m)
                                  - iLogFactorial (bigN - k) )

-- | Given model and introduced count `k`
nsInfoDelta :: Model m -> Int -> Double
nsInfoDelta mdl = nsInfoDelta_ (numSymbols mdl) (stringLen mdl)

-- | Given model and introduced count `k`
nsDelta :: Model m -> Int -> Int
nsDelta = ceiling .: nsInfoDelta

-- ss :: String

ssLen :: PrimMonad m => Model m -> m Int
ssLen = fmap ceiling . ssInfo

ssInfo :: PrimMonad m => Model m -> m Double
ssInfo (Model _ bigN ns _)
  | bigN <= 1 = return 0
  | otherwise = do
      ldenom <- Dyn.foldl' (flip ((+) . iLogFactorial)) 0.0 ns
      return $ log2e * (iLogFactorial bigN - ldenom)

-- | Given a list of n-counts before and after the introduction of a
-- type
ssInfoDelta_ :: Int -> [(Int,Int)] -> Int -> Double
ssInfoDelta_ bigN dns k
  | k /= sum (uncurry (-) <$> dns) = -- invariant
      error $ "Model.ssInfoDelta_: invalid params " ++ show (k,dns)
  | otherwise = log2e * ( iLogFactorial (bigN - k)
                          + sum (iLogFactorial <$> ns)
                          - iLogFactorial bigN
                          - iLogFactorial k -- n
                          - sum (iLogFactorial <$> ns'))
  where (ns,ns') = unzip dns

-- | Given the model, count of the introduced JointType and counts of
-- covered Joints
ssInfoDelta :: PrimMonad m => Model m -> Int -> Map (Sym,Sym) Int -> m Double
ssInfoDelta (Model _ bigN ns _) k jtnm = do
  dns <- forM (IM.toAscList im) $ \(s,d) ->
    Dyn.read ns s >>= \n -> return (n, n-d)
  return $ ssInfoDelta_ bigN dns k
  where
    jtns = M.toAscList jtnm
    im0 = IM.fromAscListWith (+) $ first fst <$> jtns
    im1 = IM.fromListWith (+) $ first snd <$> jtns
    im = IM.unionWith (+) im0 im1

-- rs :: Resolutions

rsLen :: PrimMonad m => Model m -> m Int
rsLen (Model ts _ _ mks) = do
  vs <- fromIntegral . JT.variety <<$>> Dyn.toList ts
  ks <- fromIntegral <<$>> Dyn.toList mks
  return $ sum $ flip2 zipWith ks vs $ \k v ->
    k * ceiling (log2 v) -- ceiling at each b.c. they can't be encoded
                         -- together (earlier k's depend on later k's)

rsInfo :: PrimMonad m => Model m -> m Double
rsInfo = fmap fromIntegral . rsLen

rsInfoDelta_ :: Int -> Int -> Double
rsInfoDelta_ k v = fromIntegral k * log2 (fromIntegral v)

rsInfoDelta :: Int -> JointType -> Double
rsInfoDelta k = rsInfoDelta_ k . JT.variety
