-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance6 where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable)

data Test a = Test (Tuple a a)
derive instance Foldable Test
