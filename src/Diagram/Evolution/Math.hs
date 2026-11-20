module Diagram.Evolution.Math (module Diagram.Evolution.Math) where

import Data.Tuple.Extra (both)
import Diagram.Information (iLogFactorial)

-- | log(x!)
logFact :: Int -> Double
logFact = iLogFactorial

-- | Natural logarithm of an integer
ilog :: Int -> Double
ilog = log . fromIntegral

-----------
-- INTRO --
-----------

-- | Compute the info delta of the introduction of a joint type given
-- parameters, ignoring effects on the encoding of parameters `m` and
-- `N`, as well as the cost to describe the joint type itself
-- (`tDelta`).
dInfo :: Int -> Int -> Int -> [(Int,Int)] -> Int -> Double
dInfo m bigN nm ils vm = nDelta + sDelta + rDelta
  where (nDelta, sDelta, rDelta) = dInfoComponents m bigN nm ils vm

-- | Return three components (terms) of the difference in information of
-- the modeled encoding of the string, omiting changes to parameters `m`
-- and `N`, and the cost to describe the joint type itself (`tDelta`).
dInfoComponents :: Int -> Int -> Int -> [(Int,Int)] -> Int ->
                   (Double, Double, Double)
dInfoComponents m bigN nm ils vm = (nDelta, sDelta, rDelta)
  where
    mpN = m + bigN
    nDelta = logFact (mpN - nm) - ilog m - logFact (mpN - 1)
    sDelta = sum (uncurry (-) . both logFact <$> ils)
             - logFact nm
    rDelta = fromIntegral nm * ilog vm

---------
-- MUT --
---------

-- | Compute the difference in the info delta from changing parameters:
-- joint type count n_m (before,after), symbol (new) counts ns
-- (before,after, i.e. n',n''), and joint type variety
-- (before,after). Doesn't include difference in the cost to encode the
-- joint type (`tDelta`).
ddInfo :: Int -> Int -> [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Double
ddInfo m bigN ils' nms vms = nDD + sDD + rDD
  where (nDD, sDD, rDD) = ddInfoComponents m bigN ils' nms vms

ddInfoComponents :: Int -> Int -> [(Int,Int)] -> (Int,Int) -> (Int,Int) ->
                    (Double, Double, Double)
ddInfoComponents m bigN ils' (nm,nm') (vm,vm') = (nDD, sDD, rDD)
  where
    mpN = m + bigN
    nDD = logFact (mpN - nm') - logFact (mpN - nm)

    sDD = logFact nm - logFact nm'
          + sum (uncurry (-) . both logFact <$> ils')
          -- (`logFact ni` (old symbol count) cancel out)

    rDD = rInfo' - rInfo
    rInfo' = fromIntegral nm' * ilog vm' -- rInfo == rDelta
    rInfo = fromIntegral nm * ilog vm -- rInfo == rDelta

-- LOSS --

-- | Compute the part of the loss function (delta delta (d2)
-- information, minus the terms independent of mutation) that is
-- dependent on the difference in joint counts incurred by the mutation
dnmLoss :: Int -> Int -> Int -> Int -> Int -> Double
dnmLoss m bigN nm vm' dnm = nLoss + sLossA + rLoss
  where
    nm' = nm + dnm
    nLoss = logFact (m + bigN - nm')
    sLossA = -logFact nm'
    rLoss | nm' == 0 = 0
          | otherwise = fromIntegral nm' * ilog vm'

-- | Compute the part of the loss function (delta delta information,
-- minus the terms independent of mutation) that is dependent on the
-- difference in symbol counts (before, after) incurred by the mutation
dnsLoss :: [(Int,Int)] -> Double
dnsLoss = sum . fmap (uncurry (-) . both logFact)

-- | Update the dnsLoss with new symbol counts differences (before,
-- after). NOTE: this accumulates float error.
ddnsLoss :: Double -> [(Int,Int)] -> Double
ddnsLoss loss ils = loss + sum (uncurry (-) . both logFact <$> ils)

-- old --

-- | Loss function is the delta-delta-info (ddInfo) of mutations without
-- the terms which are constant accross all mutations
ddLoss :: Int -> Int -> [(Int,Int)] -> Int -> Int -> Double
ddLoss m bigN ils' nm' vm' = nLoss + sLoss + rLoss
  where
    nLoss = logFact (m + bigN - nm')
    sLoss = -logFact nm' + sum (uncurry (-) . both logFact <$> ils')
    rLoss = fromIntegral nm' * ilog vm'

-- | Return the difference between the deltaDeltaInfo and the mutLoss
-- (ddLoss). This is the sum of the terms of deltaDeltaInfo which are
-- constant across all mutations.
ddLossComplement :: Int -> Int -> Int -> Int -> Double
ddLossComplement m bigN nm vm = nLossC + sLossC + rLossC
  where
    nLossC = - logFact (m + bigN - nm)
    sLossC = logFact nm
    rLossC = - (fromIntegral nm * ilog vm)
