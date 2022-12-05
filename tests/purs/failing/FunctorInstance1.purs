-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FunctorInstance1 where

import Prelude
import Data.Predicate (Predicate)

data Test a = Test (Predicate a)
derive instance Functor Test
