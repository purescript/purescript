module A where

  thing :: Int
  thing = 1

module B where

  thing :: Int
  thing = 2

module Main where

  -- No error as we never force `thing` to be resolved in `Main`
  import A
  import B

  main = Control.Monad.Eff.Console.log "Done"
