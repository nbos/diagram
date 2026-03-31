{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
module Diagram.Sites (module Diagram.Sites) where

import Control.Monad
import Control.Lens hiding (Index)
import Control.Monad.State.Strict

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Strict.Tuple (Pair((:!:)),(:!:))

import Diagram.UnionType (Sym)
import Diagram.Doubly (Index)

import Diagram.Util

type Len = Int
data Sites = Sites
  { _counts :: !(IntMap Int) -- :: s --> n
  , _heads  :: !(IntMap (Len :!: (Index, Sym))) -- :: hd --> (len, (tl,stl))
  , _tails  :: !(IntMap (Len :!: Index)) }      -- :: tl --> (len, hd)
  deriving(Show,Eq)
makeLenses ''Sites

join :: Sites -> Sites -> Sites
join sitesA sitesB = runIdentity $ flip evalStateT (sitesA :!: sitesB) $ do
  ns <- uses2 (_1.counts) (_2.counts) $ IM.unionWith (+)
  cAB <- uses2 (_1.tails) (_2.heads) $ IM.intersectionWith (,)
  ns' <- go ns $ IM.toList cAB
  cBA <- uses2 (_2.tails) (_1.heads) $ IM.intersectionWith (,)
  ns'' <- go ns' $ IM.toList cBA
  hds <- uses2 (_1.heads) (_2.heads) $ IM.unionWithKey err
  tls <- uses2 (_1.tails) (_2.tails) $ IM.unionWithKey err
  return $ Sites ns'' hds tls

  where
    go :: IntMap Int -> [(Index, (Len :!: Index, Len :!: (Index, Sym)))] ->
          StateT (Sites :!: Sites) Identity (IntMap Int)
    go dns [] = return dns
    go dns ((i, (lenA :!: hdA, lenB :!: (tlB,stl))):rest) = do
      let lenA' = lenA + lenB - 1
      _1.heads %= IM.insert hdA (lenA' :!: (tlB,stl))
      _1.tails %= IM.delete i
      _2.heads %= IM.delete i
      _2.tails %= IM.insert tlB (lenA' :!: hdA)
      let oldTailB | even lenB = 1
                   | otherwise = 0
          newTailB | even lenA' = 1
                   | otherwise = 0
          delta = newTailB - oldTailB
          dns' | delta == 0 = dns
               | otherwise = flip2 IM.alter stl dns $ \case
                   Nothing  -> Just delta
                   Just ntl -> nothingIf (== 0) (ntl + delta)
      go dns' rest

    err :: (Show k, Show v0, Show v1) => k -> v0 -> v1 -> a
    err = error . ("Sites.join: collision: " ++) . show .:. (,,)


-- | Use the target of two lenses in the current state with a function
uses2 :: MonadState s m => Lens' s a -> Lens' s b -> (a -> b -> c) -> m c
uses2 a b = uses a >=> uses b
{-# INLINE uses2 #-}

-- | Use the target of three lenses in the current state with a function
uses3 :: MonadState s m =>
  Lens' s a -> Lens' s b -> Lens' s c -> (a -> b -> c -> d) -> m d
uses3 a b c = uses a >=> uses b >=> uses c
{-# INLINE uses3 #-}
