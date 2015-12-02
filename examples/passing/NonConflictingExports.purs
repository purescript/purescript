module A where

  thing :: Int
  thing = 1

-- No failure here as the export `thing` only refers to Main.thing
module Main (thing, main) where

  import A

  thing :: Int
  thing = 2

  main = Control.Monad.Eff.Console.log "Done"
