module Diagram.EM (module Diagram.EM) where

import System.Random (mkStdGen, randomRs)

-- | Given a seed, randomly assign values to one of two sets
split :: Int -> [a] -> ([a], [a])
split seed = go bits -- use filter?
  where
    bits = randomRs (False, True) (mkStdGen seed)
    go _ [] = ([],[])
    go (b:bs) (a:as) | b = (a:a0s, a1s)
                     | otherwise = (a0s, a:a1s)
      where (a0s,a1s) = go bs as
    go [] _ = error "split: ran out of bits"
