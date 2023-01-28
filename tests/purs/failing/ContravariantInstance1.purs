-- @shouldFailWith CannotDeriveInvalidConstructorArg
module ContravariantInstance1 where

import Data.Functor.Contravariant (class Contravariant)
import Data.Predicate (Predicate)

newtype Test a = Test (Predicate (Predicate a))

derive instance Contravariant Test
