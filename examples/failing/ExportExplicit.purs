module M1 (X(..)) where

  data X = X | Y
  data Z = Z

module Main where

  import M1
  
  testX = X
  testY = Y
  
  -- should fail as Z is not exported from M1
  testZ = Z

  main = Debug.Trace.trace "Done"
