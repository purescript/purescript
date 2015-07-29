module Main where

import Prelude
import Control.Monad.Eff.Console

f x = x * 10.0
g y = y - 10.0

main = log $ show $ (f <<< g) 100.0
