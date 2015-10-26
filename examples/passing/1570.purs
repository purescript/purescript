module Main where

test :: forall a. a -> a
test = \(x :: a) -> x

main = Control.Monad.Eff.Console.log "Done"
