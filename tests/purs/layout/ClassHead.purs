module Test where

import Foo (class Foo)

class Foo a b c d | a -> b, c -> d where
  foo :: Foo

class Foo a b c d | a -> b, c -> d

instance foo :: Foo
