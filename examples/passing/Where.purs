module Main where

import Prelude
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff
import Control.Monad.Eff.Console (logShow, log)

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

main :: Eff _ _
main = do
  logShow (test1 1.0)
  logShow (test2 1.0 2.0)
  logShow test3
  unsafePartial (logShow test4)
  logShow test5
  logShow test6
  logShow (test7 100.0)
  log "Done"
