-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude
import Data.Foldable (class Foldable)

data Foo f = Bar (f Int)

derive instance Foldable Foo
