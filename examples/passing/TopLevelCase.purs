module Main where

foreign import jsMod
  """
  function jsMod(x) {
    return function (y) {
      return x % y;
    };
  }
  """ :: Number -> Number -> Number

infixl 7 %
(%) = jsMod

gcd :: Number -> Number -> Number
gcd 0 x = x
gcd x 0 = x
gcd x y | x > y = gcd (x % y) y
gcd x y = gcd (y % x) x

guardsTest (x:xs) | x > 0 = guardsTest xs
guardsTest xs = xs

data A = A

parseTest A 0 = 0

main = Debug.Trace.trace "Done"
