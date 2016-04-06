module Main where

import Prelude
import Control.Monad.Eff.Console (print)

main = do
  print (sum 1.0 2.0)
  print (sum 1 2)

sum x y = x + y
