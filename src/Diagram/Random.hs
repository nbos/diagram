{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Diagram.Random (module Diagram.Random) where

import Control.Monad.Random hiding (split)
import qualified System.Random.Shuffle as Shuffle

-- | Randomly assign values to one of two sets
--
-- >>> import Control.Monad.Random.Lazy (evalRand,mkStdGen)
-- >>> evalRandIO (split [1::Int ..] >>= \(left, right) -> return (take 10 left, take 10 right))
-- ([2,4,5,6,7,9,11,14,17,18],[1,3,8,10,12,13,15,16,19,20])
-- ([5,7,8,9,10,12,13,14,15,16],[1,2,3,4,6,11,18,21,22,23])
-- ([1,8,9,10,11,13,20,21,32,33],[2,3,4,5,6,7,12,14,15,16])
-- ([2,3,4,6,7,8,9,10,11,12],[1,5,14,15,17,19,21,23,24,25])
split :: MonadRandom m => [a] -> m ([a], [a])
split [] = return ([], [])
split (a:as) = do
  b <- getRandom @_ @Bool
  ~(a0s, a1s) <- split as
  return $ if b then (a0s, a:a1s) else (a:a0s, a1s)

-- | Given list and its length, uniformly shuffle the list
--
-- >>> import Control.Monad.Random.Lazy (evalRand,mkStdGen)
-- >>> str = "Hello World"
-- >>> evalRandIO (shuffle (length str) str)
-- "W dloHrelol"
-- "rd ellWolHo"
-- "o eHlrdWoll"
-- "elldo olrWH"
-- " dHollreWlo"
shuffle :: MonadRandom m => Int -> [a] -> m [a]
shuffle len as = Shuffle.shuffle as <$>
                 mapM (getRandomR . (0,)) [len-1,len-2..1]
