module Diagram.Evolution.Mutation (module Diagram.Evolution.Mutation) where

import Diagram.String

data Mutation = AddLeft  !Sym
              | AddRight !Sym
              | Add2     !Sym !Sym
              | DelLeft  !Sym
              | DelRight !Sym
              | Del2     !Sym !Sym
  deriving(Show,Eq,Ord)

-- IMPORTANT: Ord instance is assumed to preserve order of arg symbols
-- within a given constructor in Diagram.Evolution.TypeState.deltaMut
-- (M.fromDistinctAscList)

-- | Sign of a mutation (Add/Del)
data MutType = Add | Del
  deriving(Show,Eq,Ord)

-- | Sign of a mutation (Add/Del)
typeOfMut :: Mutation -> MutType
typeOfMut (AddLeft _)  = Add
typeOfMut (AddRight _) = Add
typeOfMut (Add2 _ _)   = Add
typeOfMut (DelLeft _)  = Del
typeOfMut (DelRight _) = Del
typeOfMut (Del2 _ _)   = Del

-- | Inverse of the given mutation
recip :: Mutation -> Mutation
recip (AddLeft s0)  = DelLeft s0
recip (AddRight s1) = DelRight s1
recip (Add2 s0 s1)  = Del2 s0 s1
recip (DelLeft s0)  = AddLeft s0
recip (DelRight s1) = AddRight s1
recip (Del2 s0 s1)  = Add2 s0 s1
