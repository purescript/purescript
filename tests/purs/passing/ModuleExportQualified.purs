module Main where

import Prelude
import Effect.Console (log, logShow)
import A as B

main = do
  logShow (B.show 1.0)
  log "Done"
