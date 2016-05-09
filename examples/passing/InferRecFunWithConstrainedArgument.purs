module Main where

import Prelude

test 100 = 100
test n = test(1 + n)

main = Control.Monad.Eff.Console.print $ test 0
