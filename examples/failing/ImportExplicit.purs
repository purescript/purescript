module M1 where

  data X = X | Y
  data Z = Z

module Main where

  import M1 (X(..))
  
  testX = X
  testY = Y
  testZ = Z

  main = Debug.Trace.trace "Done"
