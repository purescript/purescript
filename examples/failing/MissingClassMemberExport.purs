-- @shouldFailWith TransitiveExportError
module Test (Foo) where

import Prelude

class Foo a where
  bar :: a -> a
