module Main where

import Prelude
import Control.Monad.Eff.Console (log, logShow)

f x y =
  let
    g = case y of
	  0.0 -> x
          x -> 1.0 + x * x
  in g + x + y

main = do
  logShow $ f 1.0 10.0
  log "Done"
