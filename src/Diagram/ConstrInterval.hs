{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE BangPatterns, LambdaCase, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
module Diagram.ConstrInterval (module Diagram.ConstrInterval) where

import Prelude as P
import Control.Lens hiding (Index,(:>))
import qualified Streaming.Prelude as S

import Diagram.Primitive
import Diagram.String
import qualified Diagram.Doubly as D

data CI = CI { _headIndex  :: !Index
             , _headSymbol :: !Sym
             , _ciLength   :: !Len
             , _tailIndex  :: !Index
             , _tailSymbol :: !Sym }
  deriving(Show,Eq)
makeLenses ''CI

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
extension :: PrimMonad m => Doubly (PrimState m) -> CI -> m [(Index,Sym)]
extension _ (CI hd shd 2 tl stl) = return [(hd,shd),(tl,stl)]
extension str (CI hd shd len _ _)
  | len < 3 = error $ "CI.extension: invalid length: " ++ show len
  | otherwise = fmap ((hd,shd):) $
                S.toList_ . S.take (len-1) . D.streamWithKeyFrom str
                =<< D.unsafeNext str hd
