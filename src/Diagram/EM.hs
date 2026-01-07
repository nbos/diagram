{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Diagram.EM (module Diagram.EM) where

import Control.Monad.Random (MonadRandom, getRandom)

-- | Randomly assign values to one of two sets
--
-- >>> import Control.Monad.Random.Lazy (evalRand,mkStdGen)
-- >>> evalRand (split [1::Int ..] >>= \(left, right) -> return (take 10 left, take 10 right)) (mkStdGen 123)
-- ([4,5,6,9,10,14,15,17,19,22],[1,2,3,7,8,11,12,13,16,18])
split :: MonadRandom m => [a] -> m ([a], [a])
split [] = return ([], [])
split (a:as) = do
  b <- getRandom @_ @Bool
  ~(a0s, a1s) <- split as
  return $ if b then (a:a0s, a1s) else (a0s, a:a1s)

