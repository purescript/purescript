module Main where

import Prelude
import Debug.Trace
import Control.Monad.Eff

doIt :: forall eff. Eff eff Boolean
doIt = return true

set = do
  trace "Testing..."
  case 0 of
    0 -> doIt
    _ -> return false

main = do
  b <- set
  case b of
    true -> trace "Done"
