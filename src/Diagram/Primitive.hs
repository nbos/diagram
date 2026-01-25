module Diagram.Primitive
  ( module Diagram.Primitive
  , PrimMonad(PrimState)
  ) where

import Control.Monad.Primitive (PrimMonad(PrimState))
import qualified Data.Vector.Generic.Mutable as MV

import qualified Data.Vector.Mutable           as Boxed
import qualified Data.Vector.Unboxed.Mutable   as Unboxed
import qualified Data.Vector.Primitive.Mutable as Primitive
import qualified Data.Vector.Storable.Mutable  as Storable

type Boxed     = Boxed.MVector
type Unboxed   = Unboxed.MVector
type Primitive = Primitive.MVector
type Storable  = Storable.MVector

newPrimRef :: (PrimMonad m, MV.MVector v a) => a -> m (v (PrimState m) a)
newPrimRef = MV.replicate 1

modifyPrimRef :: (PrimMonad m, MV.MVector v a) => v (PrimState m) a -> (a -> a) -> m ()
modifyPrimRef v f = MV.unsafeModify v f 0

readPrimRef :: (PrimMonad m, MV.MVector v a) => v (PrimState m) a -> m a
readPrimRef = flip MV.unsafeRead 0
