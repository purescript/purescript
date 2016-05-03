module Main where

import Prelude
import Control.Monad.Eff.Console (log)

f x = x * 10.0
g y = y - 10.0

main = do
  log $ show $ (f <<< g) 100.0
  log "Done"
