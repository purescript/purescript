module Main where

foo = bar
  where bar r = r + 1.0

r = foo 2.0

main = Debug.Trace.trace "Done"
