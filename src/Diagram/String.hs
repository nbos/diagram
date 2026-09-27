module Diagram.String (module Diagram.String, Index) where

import Data.Vector.Unboxed.Mutable (MVector)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

import Diagram.Doubly (Index)
import qualified Diagram.Doubly as D
import Diagram.Util

type Sym = Int
type Len = Int
type Count = Int

type Doubly s = D.Doubly MVector s Sym

-- | Union of counts on respective keys, deleting those that get to
-- zero.
imUnion :: IntMap Int -> IntMap Int -> IntMap Int
imUnion = IM.mergeWithKey (const $ nothingIf (==0) .: (+)) id id
