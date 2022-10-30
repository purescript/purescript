-- @shouldFailWith CannotDeriveInvalidConstructorArg
module FoldableInstance6 where

import Prelude
import Data.Foldable (class Foldable)

data Test f a = Test (f a a)
derive instance Foldable (Test f)
