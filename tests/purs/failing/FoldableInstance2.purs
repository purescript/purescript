-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Data.Foldable (class Foldable)

data Foo :: (Type -> Type) -> Type
data Foo a = Bar

derive instance Foldable Foo
