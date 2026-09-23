{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrInterval (module Diagram.ConstrInterval) where

import Prelude as P
import Control.Lens hiding (Index,(:>))
import qualified Streaming.Prelude as S

import qualified Data.List as L
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Primitive
import Diagram.String
import qualified Diagram.Doubly as D

data CI = CI { _headIndex  :: !Index
             , _headSymbol :: !Sym
             , _ciLength   :: !Len
             , _tailIndex  :: !Index
             , _tailSymbol :: !Sym }
  deriving(Eq,Ord)
makeLenses ''CI

instance Show CI where
  show :: CI -> String
  show (CI hd shd len tl stl) = "CI " ++ show hd
                                ++ " " ++ show shd
                                ++ " " ++ show len
                                ++ " " ++ show tl
                                ++ " " ++ show stl

-- | Construct an interval from two successive index-symbol pairs.
singleton :: (Index,Sym) -> (Index,Sym) -> CI
singleton (hd,shd) (tl,stl) = CI hd shd 2 tl stl

-- | Join two successive intervals. Only works if tail index (and
-- symbol) of the first given interval is the head index (and symbol) of
-- the second. Returns Nothing otherwise.
join :: CI -> CI -> Maybe CI
join a b | a^.tailIndex == b^.headIndex = Just $ unsafeJoin a b
         | otherwise = Nothing

-- | Assumes tail index (and symbol) of the first given interval is the
-- head index (and symbol) of the second.
unsafeJoin :: CI -> CI -> CI
unsafeJoin (CI hd shd lenA _ _) (CI _ _ lenB tl stl) =
  CI hd shd (lenA + lenB - 1) tl stl
{-# INLINE unsafeJoin #-}

-- | Returns True iff the length of the interval is *evn*, meaning that
-- the last symbol *is* constructive.
even :: CI -> Bool
even (CI _ _ len _ _) = P.even len

-- | Returns True iff the length of the interval is *odd*, meaning that
-- the last symbol is *not* constructive
odd :: CI -> Bool
odd (CI _ _ len _ _) = P.odd len

-- | Given the reference string and a contructive interval, produce the
-- list of indexed symbols that form the interval, starting at the head
-- and ending at the tail.
symExtension :: PrimMonad m => Doubly (PrimState m) -> CI -> m [(Index,Sym)]
symExtension _ (CI hd shd 2 tl stl) = return [(hd,shd),(tl,stl)]
symExtension str (CI hd shd len _ _)
  | len < 3 = error $ "CI.symExtension: invalid length: " ++ show len
  | otherwise = fmap ((hd,shd):) $
                S.toList_ . S.take (len-1) . D.streamWithKeyFrom str
                =<< D.unsafeNextKey str hd

symCounts :: PrimMonad m => Doubly (PrimState m) -> CI -> m (IntMap Count)
symCounts _ (CI _ shd 2 _ stl) =
  return $ IM.insertWith (+) shd 1 $ IM.singleton stl 1
symCounts str ci@(CI _ _ len _ _) = (<$> symExtension str ci) $
  L.foldl' (flip $ uncurry $ IM.insertWith (+)) IM.empty
  . fmap (\(_,s) -> (s,1)) -- (ix,sym) -> (sym,1)
  . (if P.even len then id else init) -- don't count last if odd

-- | Return the joints in a CI, including non-constructive ones.
jointExtension :: PrimMonad m => Doubly (PrimState m) -> CI -> m [(Index,(Sym,Sym))]
jointExtension _ (CI hd shd 2 _ stl) = return [(hd,(shd,stl))]
jointExtension str (CI hd shd len _ _)
  | len < 3 = error $ "CI.jointExtension: invalid length: " ++ show len
  | otherwise = do
      iss <- fmap ((hd,shd):) $
             S.toList_ . S.take (len-1) . D.streamWithKeyFrom str
             =<< D.unsafeNextKey str hd
      let (is,ss) = unzip iss
      return $ zip is $ zip ss (drop 1 ss)

--------------
-- SUPER-CI --
--------------

-- TODO: rewrite this doc
-- | For a reference string, membership functions for (1) a super-type
-- and (2) a sub-type, and a CI that maximally (meaning it couldn't be
-- extended without falling out of it) inhabits the sub-type, return the
-- super-CI of the given CI in the super-type (i.e. its maximal
-- extension within the super-type), but only if this super-CI doesn't
-- contain another sub-CI on the left of the given CI (for injectivity).
--
-- Explicitly: returns `Nothing` if the superCI is canonically mapped on
-- by another CI on its left (canceled), `Just Nothing` if the superCI
-- is itself (doesn't extend further), and `Just (Just _)` if there is a
-- strictly larger CI in the super-type to return.
--
-- Returns the superCI (fst) as well as the remainder CIs from taking
-- aways all of the given JointType's intervals from the superCI (snd),
-- in left-to-right order.
superCI :: forall m. PrimMonad m => Doubly (PrimState m) ->
  (Sym -> Sym -> m Bool) -> (Sym -> Sym -> m Bool) ->
  CI -> m (Maybe (Maybe (CI, [CI])))
superCI dly super sub (CI hd0 shd0 len0 tl0 stl0) = do

  bwd <- (D.prev dly hd0 >>=) $ \case
    Nothing -> return $ Just Nothing -- same
    Just (phd, sphd) -> (super sphd shd0 >>=) $ \case
      False -> return $ Just Nothing -- same
      True -> goBwd hd0 shd0 phd sphd 2 -- tl first

  case bwd of
    Nothing -> return Nothing -- canceled (escaladed from expandBwd)
    Just bwd' -> do
      fwd <- (D.next dly tl0 >>=) $ \case
        Nothing -> return Nothing -- same
        Just (ntl, sntl) -> (super stl0 sntl >>=) $ \case
          False -> return Nothing -- same
          True -> Just <$> goFwd tl0 stl0 2 ntl sntl -- GT

      return $ Just $ case (bwd', fwd) of
        (Nothing, Nothing) -> Nothing -- same: Just Nothing
        (Nothing, Just (CI _ _ lenFwd tl stl, rems)) ->
          let len = len0 + lenFwd - 1
          in Just (CI hd0 shd0 len tl stl, rems)
        (Just lrem@(CI hd shd lenBwd _ _), Nothing) ->
          let len = lenBwd + len0 - 1
          in Just (CI hd shd len tl0 stl0, [lrem])
        (Just lrem@(CI hd shd lenBwd _ _), Just (CI _ _ lenFwd tl stl, rems)) ->
          let len = lenBwd + len0 + lenFwd - 2
          in Just (CI hd shd len tl stl, lrem:rems)
  where
    goBwd tl stl = go
      where
        go hd shd !len = (D.prev dly hd >>=) $ \case
          Nothing -> return $ Just $ Just ci -- eos
          Just (phd, sphd) -> (super sphd shd >>=) $ \case
            False -> return $ Just $ Just ci -- end
            True -> (sub sphd shd >>=) $ \case
              True -> return Nothing -- canceled
              False -> go phd sphd (len+1) -- continue
          where ci = CI hd shd len tl stl

    goFwd hd shd = goRem [] 1 hd shd -- (len+remLen-1) overlap logic
      where                          -- requires we start with len 1
        goRem rems len remHd remShd = go
          where -- a remainder is inside TypeState but outside jt
            go !remLen tl stl = (D.next dly tl >>=) $ \case
              Nothing -> return res -- eos
              Just (ntl, sntl) -> (super stl sntl >>=) $ \case
                True -> (sub stl sntl >>=) $ \case
                  True -> goJT rems' (len+remLen-1) ntl sntl -- switch
                  False -> go (remLen+1) ntl sntl -- cont.
                False -> return res -- end
              where res = ( CI hd shd (len+remLen-1) tl stl
                          , reverse rems' )
                    rems' = (CI remHd remShd remLen tl stl):rems

        goJT rems !len tl stl = (D.next dly tl >>=) $ \case
          Nothing -> return res -- eos
          Just (ntl, sntl) -> (super stl sntl >>=) $ \case
            True -> (sub stl sntl >>=) $ \case
              True -> goJT rems (len+1) ntl sntl -- cont.
              False -> goRem rems len tl stl 2 ntl sntl -- switch
            False -> return res -- end
          where res = ( CI hd shd len tl stl
                      , reverse rems )
