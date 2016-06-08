module Main where

import Control.Monad.Eff.Console (log)

test :: forall a. a -> a
test = \(x :: a) -> x

main = log "Done"
