module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

test =
  let f :: Number -> Boolean
      f 0.0 = false
      f n = g (n - 1.0)

      g :: Number -> Boolean
      g 0.0 = true
      g n = f (n - 1.0)

      x = f 1.0
  in not x

main = do
  logShow test
  log "Done"
