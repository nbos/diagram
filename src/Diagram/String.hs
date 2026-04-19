module Diagram.String (Sym, Doubly, Index) where

import Data.Vector.Unboxed.Mutable (MVector)
import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D

type Sym = Int
type Doubly s = D.Doubly MVector s Sym
