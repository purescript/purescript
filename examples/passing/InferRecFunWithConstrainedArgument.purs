module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

test 100.0 = 100.0
test n = test(1.0 + n)

main = do
  logShow $ test 0.0
  log "Done"
