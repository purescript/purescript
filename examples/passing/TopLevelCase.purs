module Main where

import Prelude

gcd :: Number -> Number -> Number
gcd 0.0 x = x
gcd x 0.0 = x
gcd x y | x > y = gcd (x `mod` y) y
gcd x y = gcd (y `mod` x) x

guardsTest [x] | x > 0.0 = []
guardsTest xs = xs

data A = A

parseTest A 0.0 = 0.0

main = Control.Monad.Eff.Console.log "Done"
