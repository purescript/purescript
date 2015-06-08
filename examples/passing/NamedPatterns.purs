module Main where

import Prelude

foo = \x -> case x of
  y@{ foo = "Foo" } -> y
  y -> y

main = Debug.Trace.trace "Done"
