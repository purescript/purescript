module M1 (X(X), Z) where

  data X = X | Y
  data Z = Z

module Main where

  import M1
  
  testX = X
  testZ = Z

  main = Debug.Trace.trace "Done"
