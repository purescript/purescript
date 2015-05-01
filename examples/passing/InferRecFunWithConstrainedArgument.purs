module Main where

import Prelude

test 100.0 = 100.0
test n = test(1.0 + n)

main = Debug.Trace.print $ test 0.0
