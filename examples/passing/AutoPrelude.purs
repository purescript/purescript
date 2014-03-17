module Main where

import Debug.Trace

f x = x * 10
g y = y - 10

main = trace $ show $ (f <<< g) 100
