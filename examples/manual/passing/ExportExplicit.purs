module M1 (X(X), Z(..), foo) where

  data X = X | Y
  data Z = Z

  foo :: Number
  foo = 0

  bar :: Number
  bar = 1

module Main where

  import M1

  testX = X
  testZ = Z
  testFoo = foo

  main = Control.Monad.Eff.Console.log "Done"
