module Main where

import Prelude
import Effect.Console (log)

test :: Number -> Boolean
test -1.0 = false
test _  = true

main = log "Done"
