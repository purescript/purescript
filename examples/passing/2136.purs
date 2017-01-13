module Main where

import Prelude
import Control.Monad.Eff.Console (log)

main = 
  if (negate (bottom :: Int) > top)
    then log "Fail"
    else log "Done"
