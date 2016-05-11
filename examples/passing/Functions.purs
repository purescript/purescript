module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test1 = \_ -> 0.0

test2 = \a b -> a + b + 1.0

test3 = \a -> a

main = log "Done"
