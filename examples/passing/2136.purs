module Main where

import Prelude
import Effect.Console (log)

main = 
  if (negate (bottom :: Int) > top)
    then log "Fail"
    else log "Done"
