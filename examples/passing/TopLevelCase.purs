module Main where

import Prelude

gcd :: Number -> Number -> Number
gcd 0.0 x = x
gcd x 0.0 = x
gcd x y | x > y = gcd (x % y) y
gcd x y = gcd (y % x) x

guardsTest [x] | x > 0.0 = []
guardsTest xs = xs

data A = A

parseTest A 0.0 = 0.0

main = Debug.Trace.trace "Done"
