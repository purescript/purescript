module Main where

  import Lib (class X, go)

  go' :: forall a. (X a) => a -> a
  go' = go

  main = Control.Monad.Eff.Console.log "Done"
