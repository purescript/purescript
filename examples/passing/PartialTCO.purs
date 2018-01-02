module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Partial.Unsafe (unsafePartial)

main = do
  let _ = partialTCO true 1000000
  log "Done"

partialTCO :: Partial => Boolean -> Int -> Int
partialTCO true 0 = 0
partialTCO true n = partialTCO true (n - 1)
