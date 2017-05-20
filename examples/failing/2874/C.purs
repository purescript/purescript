-- @shouldFailWith "type wildcard"

module Lib where

import Prelude

class Foo a where
  foo :: a

class Baz b where
  baz :: b

instance bazFoo :: (Baz _) => Foo b where
  foo = baz
