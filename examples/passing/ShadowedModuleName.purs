module Main where

import Test
import Control.Monad.Eff.Console

data Test = Test

main = log (runZ (Z "Done"))
