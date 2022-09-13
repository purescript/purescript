-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance6 where

import Prelude
import Data.Foldable (class Foldable)

data Test a = Test (a -> Int)
derive instance Foldable Test
