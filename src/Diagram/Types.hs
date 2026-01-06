{-# LANGUAGE ScopedTypeVariables #-}
module Diagram.Types (module Diagram.Types) where

import Prelude hiding (length,read)
import Control.Monad
import Control.Monad.Primitive (PrimMonad)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Diagram.Dynamic (BoxedVec)
import qualified Diagram.Dynamic as Dyn

import Diagram.UnionType (UnionType)
import Diagram.JointType (JointType)
import qualified Diagram.JointType as J
import Diagram.Model (Sym)

-- | Forest of joint-of-unions type refinements
data Types m = Types {
  rootUnion  :: !UnionType, -- top union of atoms only
  tsParent   :: !(BoxedVec m (Maybe Sym)),
  tsChildren :: !(BoxedVec m IntSet),
  tsTypes    :: !(BoxedVec m JointType)
}

------------------
-- CONSTRUCTION --
------------------

-- | Length 0, given a top (union) type of atoms
new :: PrimMonad m => UnionType -> m (Types m)
new top = Types top <$> Dyn.new <*> Dyn.new <*> Dyn.new

-- | Construction (empty) given a top (union) type of atoms and a
-- capacity
withCapacity :: forall m. PrimMonad m => UnionType -> Int -> m (Types m)
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

read :: PrimMonad m => Types m -> Sym -> m JointType
read (Types _ _ _ ts) = Dyn.read ts

------------
-- MODIFY --
------------

push :: PrimMonad m => Types m -> Maybe Sym -> JointType -> m (Types m)
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
checkIntegrity typs@(Types _ mps css ts) a = do
  let err = error . ("Types.checkIntegrity: " ++)
      len = Dyn.length ts

  unless (Dyn.length mps == len && Dyn.length css == len) $
    err $ "field vectors are not the same length: "
    ++ show (Dyn.length mps, Dyn.length css, Dyn.length ts)

  forM_ [0..len - 1] $ \s -> do
    mp <- parentOf typs s
    t <- read typs s
    -- unless (t `J.leq` top) $
    --   err $ "type is not a subtype of top: " ++ show (s,t,top)

    forM_ mp $ \p -> do
      pcs <- childrenOf typs p
      pt <- read typs p
      unless (IS.member s pcs) $
        err $ "symbol missing from its parent's children: "
        ++ show (s,p,pcs)
      unless (t `J.leq` pt) $
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

  -- top' <- L.foldl' J.join J.bot <$> Dyn.toList ts
  -- unless (top == top') $
  --   err $ "top type field is not the least upper bound: "
  --   ++ show (top,top')
  return a
