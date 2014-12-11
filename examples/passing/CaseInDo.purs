module Main where

import Debug.Trace
import Control.Monad.Eff

main = do
  trace "Testing..."
  case 0 of
    0 -> trace "Done"
    _ -> return unit

