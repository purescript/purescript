module Main where

  foo = \x -> case x of
    y@{ foo = "Foo" } -> y
    y -> y

  main = Debug.Trace.trace "Done"
