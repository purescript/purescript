module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test :: Number -> Boolean
test -1.0 = false
test _  = true

main = log "Done"
