module S where

  s = \x y z -> x z (y z)
    
module Main where

main = Debug.Trace.trace "Done"
