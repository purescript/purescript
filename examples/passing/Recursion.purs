module Main where

import Prelude

fib = \n -> case n of
  0.0 -> 1.0
  1.0 -> 1.0
  n -> fib (n - 1.0) + fib (n - 2.0)

main = Debug.Trace.trace "Done"
