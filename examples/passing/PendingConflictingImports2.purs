module A where

  thing :: Int
  thing = 1

module Main where

  import A

  -- No error as we never force `thing` to be resolved in `Main`
  thing :: Int
  thing = 2

  main = Control.Monad.Eff.Console.log "Done"
