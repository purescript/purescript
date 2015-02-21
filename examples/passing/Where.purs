module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST

test1 x = y
  where
  y :: Number
  y = x + 1

test2 x y = x' + y'
  where
  x' = x + 1
  y' = y + 1


test3 = f 1 2 3
  where f x y z = x + y + z


test4 = f (+) [1, 2]
  where f x [y, z] = x y z


test5 = g 10
  where
  f x | x > 0 = g (x / 2) + 1
  f x = 0
  g x = f (x - 1) + 1

test6 = if f true then f 1 else f 2
  where f :: forall a. a -> a
        f x = x

test7 :: Number -> Number
test7 x = go x
  where
  go y | (x - 0.1 < y * y) && (y * y < x + 0.1) = y
  go y = go $ (y + x / y) / 2

main = do
  Debug.Trace.print (test1 1)
  Debug.Trace.print (test2 1 2)
  Debug.Trace.print test3
  Debug.Trace.print test4
  Debug.Trace.print test5
  Debug.Trace.print test6
  Debug.Trace.print (test7 100)
