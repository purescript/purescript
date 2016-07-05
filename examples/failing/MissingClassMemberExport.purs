-- @shouldFailWith TransitiveExportError
module Test (class Foo) where

import Prelude

class Foo a where
  bar :: a -> a
