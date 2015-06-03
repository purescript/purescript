module Main where

import Prelude
import Debug.Trace

f x = x * 10.0
g y = y - 10.0

main = trace $ show $ (f <<< g) 100.0
