module Main where

import Prelude
import Effect.Console

main
  | between 0 1 2 = log "Fail"
  | otherwise = log "Done"
