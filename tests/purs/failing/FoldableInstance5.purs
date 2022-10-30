-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance5 where

import Prelude
import Data.Foldable (class Foldable)
import Data.Tuple (Tuple(..))

data Test a = Test (Tuple a Int)
derive instance Foldable Test
