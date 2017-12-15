module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console (log, logShow)

main = do
  let uint :: UInt
      uint = 1u
  -- TODO: actually print the UInt
  logShow "hello"

