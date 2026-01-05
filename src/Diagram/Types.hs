{-# LANGUAGE ScopedTypeVariables #-}
module Diagram.Types (module Diagram.Types) where

import Prelude hiding (length,read)
import Control.Monad
import Control.Monad.Primitive (PrimMonad)

import qualified Data.List as L
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Diagram.Dynamic (BoxedVec)
import qualified Diagram.Dynamic as Dyn

import Diagram.Type (Type)
import qualified Diagram.Type as T
import Diagram.Model (Sym)

-- | Forest of joint-of-unions type refinements
data Types m = Types {
  topType    :: !Type, -- join of all types
  tsParent   :: !(BoxedVec m (Maybe Sym)),
  tsChildren :: !(BoxedVec m IntSet),
  tsTypes    :: !(BoxedVec m Type)
}

------------------
-- CONSTRUCTION --
------------------

-- | Length 0, given a top type
new :: PrimMonad m => Type -> m (Types m)
new top = Types top <$> Dyn.new <*> Dyn.new <*> Dyn.new

-- | Construction (empty) given a top type and capacity
withCapacity :: forall m. PrimMonad m => Type -> Int -> m (Types m)
withCapacity top n = Types top <$> newDyn <*> newDyn <*> newDyn
  where newDyn = Dyn.withCapacity n :: m (BoxedVec m a)

------------
-- ACCESS --
------------

length :: PrimMonad m => Types m -> Int
length (Types _ _ _ ts) = Dyn.length ts

parentOf :: PrimMonad m => Types m -> Sym -> m (Maybe Sym)
parentOf (Types _ mps _ _) = Dyn.read mps

childrenOf :: PrimMonad m => Types m -> Sym -> m IntSet
childrenOf (Types _ _ css _) = Dyn.read css

read :: PrimMonad m => Types m -> Sym -> m Type
read (Types _ _ _ ts) = Dyn.read ts

------------
-- MODIFY --
------------

push :: PrimMonad m => Types m -> Maybe Sym -> Type -> m (Types m)
push typs@(Types top mps css ts) mp t = do
  let s = length typs
  forM_ mp $ Dyn.modify css $ IS.insert s
  Types top <$> Dyn.push mps mp
    <*> Dyn.push css IS.empty
    <*> Dyn.push ts t

-----------
-- DEBUG --
-----------

checkIntegrity :: PrimMonad m => Types m -> a -> m a
checkIntegrity typs@(Types top mps css ts) a = do
  let err = error . ("Types.checkIntegrity: " ++)
      len = Dyn.length ts

  unless (Dyn.length mps == len && Dyn.length css == len) $
    err $ "field vectors are not the same length: "
    ++ show (Dyn.length mps, Dyn.length css, Dyn.length ts)

  forM_ [0..len - 1] $ \s -> do
    mp <- parentOf typs s
    t <- read typs s
    unless (t `T.leq` top) $
      err $ "type is not a subtype of top: " ++ show (s,t,top)
    unless (t /= top) $
      err $ "type is equal to top: " ++ show (s,t,top)

    forM_ mp $ \p -> do
      pcs <- childrenOf typs p
      pt <- read typs p
      unless (IS.member s pcs) $
        err $ "symbol missing from its parent's children: "
        ++ show (s,p,pcs)
      unless (t `T.leq` pt) $
        err $ "type is not a subtype of its parent: "
        ++ show (s,t,p,pt)
      unless (t /= pt) $
        err $ "type is equal to its parent: "
        ++ show (s,t,p,pt)

    cs <- childrenOf typs s
    forM_ (IS.toList cs) $ \c -> do
      cmp <- parentOf typs c
      unless (cmp == Just s) $
        err $ "symbol not the parent of one of its children: "
        ++ show (s,c)

  top' <- L.foldl' T.join T.bot <$> Dyn.toList ts
  unless (top == top') $
    err $ "top type field is not the least upper bound: "
    ++ show (top,top')

  return a
