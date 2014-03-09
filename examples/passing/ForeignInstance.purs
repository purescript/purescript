module Foo where

class Foo a where
  foo :: a -> String

foreign import instance fooArray :: (Foo a) => Foo [a]

module M1 where

foreign import instance fooNumber :: Foo.Foo Number

module Main where

import M1
import Foo

foreign import instance fooString :: Foo String

test1 _ = foo [1, 2, 3]

test2 _ = foo "Test"

main = Debug.Trace.trace "Done"
