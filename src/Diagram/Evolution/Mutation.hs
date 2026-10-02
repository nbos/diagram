module Diagram.Evolution.Mutation (module Diagram.Evolution.Mutation) where

import Diagram.String
import Diagram.JointType (JointType)
import qualified Diagram.JointType as JT

data Mutation = AddLeft  !Sym
              | AddRight !Sym
              | Add2     !Sym !Sym
              | DelLeft  !Sym
              | DelRight !Sym
              | Del2     !Sym !Sym
  deriving(Show,Eq,Ord)

-- IMPORTANT: Ord instance is assumed to preserve order of arg symbols
-- within a given constructor in
-- Diagram.Evolution.TypeState.deltaMutJoints (S.fromDistinctAscList)

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

-----------
-- APPLY --
-----------

-- | Safe apply a mut
apply :: Mutation -> JointType -> JointType
apply (AddLeft  s0) = JT.insertLeft  s0
apply (AddRight s1) = JT.insertRight s1
apply (Add2  s0 s1) = JT.insertBoth  s0 s1
apply (DelLeft  s0) = JT.deleteLeft  s0
apply (DelRight s1) = JT.deleteRight s1
apply (Del2  s0 s1) = JT.deleteBoth  s0 s1

-- | Apply a mut, where any added symbols is missing from the type and
-- any deleted symbols is member of the type in their respective
-- positions.
unsafeApply :: Mutation -> JointType -> JointType
unsafeApply (AddLeft  s0) = JT.insertLeftMissing  s0
unsafeApply (AddRight s1) = JT.insertRightMissing s1
unsafeApply (Add2  s0 s1) = JT.insertBothMissing s0 s1
unsafeApply (DelLeft  s0) = JT.deleteLeftMember s0
unsafeApply (DelRight s1) = JT.deleteRightMember s1
unsafeApply (Del2  s0 s1) = JT.deleteBothMember s0 s1
