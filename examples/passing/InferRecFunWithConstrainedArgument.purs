module Main where

import Prelude

test 100 = 100
test n = test(1 + n)

main = Trace.print $ test 0
