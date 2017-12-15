module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.Console (log, logShow)

main = do
  let -- uint :: UInt -- TODO: make UInt a type that shows up
      uint = 1u
  -- TODO: actually print the UInt
  logShow "Done"

