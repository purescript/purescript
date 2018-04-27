module Main where

import Prelude
import Effect.Console (log)

foo = bar
  where bar r = r + 1.0

r = foo 2.0

main = log "Done"
