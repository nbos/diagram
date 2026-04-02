{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
module Diagram.Sites (module Diagram.Sites) where

import Control.Monad
import Control.Lens hiding (Index)
import Control.Monad.State.Strict

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Strict.Tuple (Pair((:!:)),(:!:),swap)

import Diagram.UnionType (Sym)
import Diagram.Doubly (Index)

import Diagram.Util

type Len = Int
data Sites = Sites
  { _counts      :: !(IntMap Int) -- :: s --> n
  , _heads2tails :: !(IntMap (Len :!: (Index, Sym))) -- :: hd --> (len, (tl,stl))
  , _tails2heads :: !(IntMap (Len :!: Index)) }      -- :: tl --> (len, hd)
  deriving(Show,Eq)
makeLenses ''Sites

join :: Sites -> Sites -> Sites
join sitesA sitesB = runIdentity $ flip evalStateT (sitesA :!: sitesB) $ do
  ns <- uses2 (_1.counts) (_2.counts) $ IM.unionWith (+)
  cAB <- uses2 (_2.heads2tails) (_1.tails2heads) IM.intersection
  ns' <- go ns $ IM.toList cAB

  modify swap -- A <--> B
  cBA <- uses2 (_2.heads2tails) (_1.tails2heads) IM.intersection
  ns'' <- go ns' $ IM.toList cBA
  modify swap -- B <--> A

  hds <- uses2 (_1.heads2tails) (_2.heads2tails) $ IM.unionWithKey err
  tls <- uses2 (_1.tails2heads) (_2.tails2heads) $ IM.unionWithKey err
  return $ Sites ns'' hds tls

  where
    go :: IntMap Int -> [(Index, Len :!: (Index, Sym))] ->
          StateT (Sites :!: Sites) Identity (IntMap Int)
    go dns [] = return dns
    go dns ((tlA@hdB, lenB :!: (tlB,stlB)):rest) = do
      _1.tails2heads %= IM.delete tlA -- delete tlA
      _2.heads2tails %= IM.delete hdB -- delete hdB (1/2)
      _2.tails2heads %= IM.delete tlB -- delete tlB (2/2)

      (lenA :!: hdA) <- (_1.tails2heads) `uses` (IM.! tlA)
      dns' <- ((_1.heads2tails) %%= deleteLookup tlB >>=) $ \case

        -- simple: seam [hdA..tlA) <> [hdB..tlB] ==> [hdA..tlB]
        Nothing -> do
          let lenA' = lenA + lenB - 1
          _1.heads2tails %= IM.insert hdA (lenA' :!: (tlB,stlB)) -- update hdA
          _1.tails2heads %= IM.insert tlB (lenA' :!: hdA)        -- insert tlB

          let oldInc | even lenB = 1
                     | otherwise = 0
              newInc | even lenA' = 1
                     | otherwise = 0
              delta = newInc - oldInc

          return $ if delta == 0 then dns
            else flip2 IM.alter stlB dns $ \case
            Nothing  -> Just delta -- assert (delta == 1)
            Just stln -> nothingIf (== 0) (stln + delta)

        -- fancy: seam [hdA..tlA) <> [hdB..tlB) <> [hdA2..tlA2] ==> [hdA..tlA2]
        Just (lenA2 :!: (tlA2,stlA2)) -> do
          let hdA2 = tlB -- aliases
              lenA' = lenA + lenB + lenA2 - 2
          _1.heads2tails %= IM.insert hdA (lenA' :!: (tlA2,stlA2)) -- update hdA
          _1.heads2tails %= IM.delete hdA2                 -- delete hdA2
          _1.tails2heads %= IM.insert tlA2 (lenA' :!: hdA) -- update tlA2

          let oldIncA2 | even lenA2 = 1
                       | otherwise = 0
              newIncA2 | even lenA' = 1
                       | otherwise = 0
              delta = newIncA2 - oldIncA2

          return $ (if even lenB then id else IM.adjust (+1) stlB) $
            (if delta == 0 then id else flip IM.alter stlA2 $ \case
                Nothing -> Just delta -- assert (delta == 1)
                Just stln -> nothingIf (== 0) (stln + delta)) dns

      go dns' rest -- continue

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

deleteLookup :: Sym -> IntMap a -> (Maybe a, IntMap a)
deleteLookup = IM.updateLookupWithKey (\_ _ -> Nothing)
{-# INLINE deleteLookup #-}
