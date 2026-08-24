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
