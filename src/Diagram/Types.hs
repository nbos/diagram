{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Diagram.Types (module Diagram.Types) where

import Prelude hiding (length,read)
import Control.Monad
import Control.Monad.Primitive (PrimMonad)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Diagram.Dynamic (BoxedVec)
import qualified Diagram.Dynamic as Dyn

import Diagram.JointType (Sym,JointType)
import qualified Diagram.JointType as J
import Diagram.Information
import Diagram.Util

-- | Forest of joint-of-unions type refinements
data Types m = Types {
  parents    :: !(BoxedVec m (Maybe Sym)),
  children   :: !(BoxedVec m IntSet),
  jointTypes :: !(BoxedVec m JointType)
}

length :: PrimMonad m => Types m -> Int
length (Types _ _ ts) = Dyn.length ts

parentOf :: PrimMonad m => Types m -> Sym -> m (Maybe Sym)
parentOf (Types mps _ _) = Dyn.read mps . (+(-256))

childrenOf :: PrimMonad m => Types m -> Sym -> m IntSet
childrenOf (Types _ css _) = Dyn.read css . (+(-256))

read :: PrimMonad m => Types m -> Sym -> m JointType
read (Types _ _ ts) = Dyn.read ts . (+(-256))

------------------
-- CONSTRUCTION --
------------------

-- | Length 0 initialization
new :: PrimMonad m => m (Types m)
new = Types <$> Dyn.new <*> Dyn.new <*> Dyn.new

-- | Construction (empty) given a capacity
withCapacity :: forall m. PrimMonad m => Int -> m (Types m)
withCapacity n = Types <$> newDyn <*> newDyn <*> newDyn
  where newDyn = Dyn.withCapacity n :: m (BoxedVec m a)

------------
-- MODIFY --
------------

push :: PrimMonad m => Types m -> Maybe Sym -> JointType -> m (Types m)
push typs@(Types mps css ts) mp t = do
  let s = length typs
  forM_ mp $ Dyn.modify css $ IS.insert s
  Types <$> Dyn.push mps mp
    <*> Dyn.push css IS.empty
    <*> Dyn.push ts t

-----------------
-- INFORMATION --
-----------------

information :: PrimMonad m => Types m -> m Double
information typs@(Types mps _ _) = do
  refineLens <- flip2 Dyn.ifoldM' 0 mps $ \acc i -> \case
    Nothing -> let s = 256 + i in return $ acc + s
    Just p -> (acc+) . J.refineLen <$> read typs p
  return $ parentsInfo + fromIntegral refineLens
  where
    len = length typs
    parentsInfo = iLogFactorial len

-----------
-- DEBUG --
-----------

checkIntegrity :: PrimMonad m => Types m -> a -> m a
checkIntegrity typs@(Types mps css ts) a = do
  let err = error . ("Types.checkIntegrity: " ++)
      len = Dyn.length ts

  unless (Dyn.length mps == len && Dyn.length css == len) $
    err $ "field vectors are not the same length: "
    ++ show (Dyn.length mps, Dyn.length css, Dyn.length ts)

  forM_ [0..len - 1] $ \i -> do
    let s = 256 + i
    mp <- parentOf typs s
    t <- read typs s
    -- let J u0 u1 = t
    -- unless (fst (IS.split 256 $ U.set u0)
    --          `IS.isSubsetOf` U.set top) $
    --   err $ "left union contains atoms not in top: "
    --   ++ show (u0,top)
    -- unless (fst (IS.split 256 $ U.set u1)
    --         `IS.isSubsetOf` U.set top) $
    --   err $ "right union contains atoms not in top: "
    --   ++ show (u1,top)

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
