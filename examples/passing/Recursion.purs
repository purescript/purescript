module Main where

  import Prelude

  fib = \n -> case n of
    0 -> 1
    1 -> 1
    n -> fib (n - 1) + fib (n - 2)

  main = Debug.Trace.trace "Done"
