module Main where

import Prelude
import Debug.Trace

bar :: Number -> Number -> Number
bar n m = n + m

foo = x `bar` y
  where
  x = 1
  y = []
