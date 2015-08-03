-- @shouldFailWith TransitiveExportError
module Test (bar) where

import Prelude

class Foo a where
  bar :: a -> a
