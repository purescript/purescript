module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console (log, logShow)

main = do
  let uint :: UInt
      uint = 1u
  -- TODO: Use the version of Prelude that includes the Show instance
  -- logShow uint
  log "Done"
