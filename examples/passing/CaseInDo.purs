module Main where

import Debug.Trace
import Control.Monad.Eff

foreign import doIt "function doIt() { global.flag = true; }" :: forall eff. Eff eff Unit

foreign import get "function get() { return global.flag; }" :: forall eff. Eff eff Boolean

set = do
  trace "Testing..."
  case 0 of
    0 -> doIt
    _ -> return unit

main = do
  set
  b <- get
  case b of
    true -> trace "Done"
