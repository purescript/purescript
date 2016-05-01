module Main where

import Prelude
import Control.Monad.Eff.Console (logShow)

main = do
  logShow (sum 1.0 2.0)
  logShow (sum 1 2)

sum x y = x + y
