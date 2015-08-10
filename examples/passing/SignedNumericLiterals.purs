module Main where

import Prelude

p = 0.5
q = 1.0
x = -1.0
y = -0.5
z = 0.5
w = 1.0

f :: Number -> Number
f x = -x

test1 = 2.0 - 1.0

main = Control.Monad.Eff.Console.log "Done"
