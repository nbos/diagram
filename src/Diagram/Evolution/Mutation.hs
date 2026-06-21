module Diagram.Evolution.Mutation (module Diagram.Evolution.Mutation) where

import Diagram.String

data Mutation = AddLeft  !Sym
              | AddRight !Sym
              | Add2     !Sym !Sym
              | DelLeft  !Sym
              | DelRight !Sym
              | Del2     !Sym !Sym
  deriving(Show,Eq,Ord)

data MutType = Add | Del

typeOf :: Mutation -> MutType
typeOf (AddLeft _)  = Add
typeOf (AddRight _) = Add
typeOf (Add2 _ _)   = Add
typeOf (DelLeft _)  = Del
typeOf (DelRight _) = Del
typeOf (Del2 _ _)   = Del
