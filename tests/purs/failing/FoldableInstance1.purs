-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Data.Foldable (class Foldable)

data Foo = Bar

derive instance Foldable Foo
