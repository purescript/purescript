module Main where

foo = bar
  where bar r = r + 1

r = foo 2

main = Debug.Trace.trace "Done"
