module Main where

import Control.Monad.Eff
import Debug.Trace
import Assert

main = do
  let x = 0.0
  assert $ x == 0.0
  let x = 1.0 + 1.0
  trace "Done"
