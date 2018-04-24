module Main where

import Prelude
import Effect.Console (logShow, log)

main = do
  logShow (sum 1.0 2.0)
  logShow (sum 1 2)
  log "Done"

sum x y = x + y
