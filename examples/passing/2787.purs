module Main where

import Prelude
import Control.Monad.Eff.Console

main
  | between 0 1 2 = log "Fail"
  | otherwise = log "Done"
