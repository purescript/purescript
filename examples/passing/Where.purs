module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

test1 x = y
  where
  y :: Number
  y = x + 1.0

test2 x y = x' + y'
  where
  x' = x + 1.0
  y' = y + 1.0


test3 = f 1.0 2.0 3.0
  where f x y z = x + y + z


test4 = f (+) [1.0, 2.0]
  where f x [y, z] = x y z


test5 = g 10.0
  where
  f x | x > 0.0 = g (x / 2.0) + 1.0
  f x = 0.0
  g x = f (x - 1.0) + 1.0

test6 = if f true then f 1.0 else f 2.0
  where f :: forall a. a -> a
        f x = x

test7 :: Number -> Number
test7 x = go x
  where
  go y | (x - 0.1 < y * y) && (y * y < x + 0.1) = y
  go y = go $ (y + x / y) / 2.0

main = do
  Control.Monad.Eff.Console.print (test1 1.0)
  Control.Monad.Eff.Console.print (test2 1.0 2.0)
  Control.Monad.Eff.Console.print test3
  Control.Monad.Eff.Console.print test4
  Control.Monad.Eff.Console.print test5
  Control.Monad.Eff.Console.print test6
  Control.Monad.Eff.Console.print (test7 100.0)
