module Main where

  s = \x y z -> x z (y z)

  main = Debug.Trace.trace "Done"
