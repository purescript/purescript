-- @shouldFailWith CannotDeriveInvalidConstructorArg
module BifunctorInstance1 where

import Prelude
import Data.Bifunctor (class Bifunctor)
import Data.Predicate (Predicate)
import Data.Tuple (Tuple)

data Test a b = Test (Tuple (Predicate a) (Predicate b)) (Tuple a b)
derive instance Bifunctor Test
