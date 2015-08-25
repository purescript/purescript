-- from #1244
module Foo where

  foo :: Int
  foo = 3

module Bar (module Foo) where

  import Foo

module Baz where

  import Bar (foo)

  baz :: Int
  baz = foo
