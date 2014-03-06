module M1 (X(X), Z(..), foo) where

  data X = X | Y
  data Z = Z
  
  foo = 0

module Main where

  import M1
  
  testX = X
  testZ = Z
  testFoo = foo

  main = Debug.Trace.trace "Done"
