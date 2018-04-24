module Main where

import Effect.Console (log)

test :: forall a. a -> a
test = \(x :: a) -> x

main = log "Done"
