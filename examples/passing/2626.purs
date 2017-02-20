module Main where

import Control.Monad.Eff.Console (log)

f = \(x :: forall a. a -> a) -> x x

test1 = (f \x -> x) 1

g = \(x :: (forall a. a -> a) -> Int) -> x (\y -> y)

test2 = g \f -> if f true then f 0 else f 1

main = log "Done"
