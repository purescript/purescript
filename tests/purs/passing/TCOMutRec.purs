module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assertEqual, assertThrows)

tco1 :: Int -> Int
tco1 = f 0
  where
  f x y = g (x + 2) (y - 1)
    where
    g x' y' = if y' <= 0 then x' else f x' y'

tco2 :: Int -> Int
tco2 = f 0
  where
  f x y = g (x + 2) (y - 1)
    where
    g x' y' = h (y' <= 0) x' y'
    h test x' y' = if test then x' else f x' y'

tco3 :: Int -> Int
tco3 y0 = f 0 y0
  where
  f x y = g x (h y)
    where
    g x' y' =
      if y' <= 0 then x'
      else if y' > y0 / 2 then g (j x') (y' - 1)
      else f (x' + 2) y'
    h y = y - 1
  j x = x + 3

tco4 :: Int -> Int
tco4 = f 0
  where
  f x y = if y <= 0 then x else g (y - 1)
    where
    g y' = f (x + 2) y'

-- The following examples are functions which are prevented from being TCO'd
-- because the arity of the function being looped does not match the function
-- call. In theory, these could be made to optimize via eta-expansion in the
-- future, in which case the assertions can change.

ntco1 :: Int -> Int
ntco1 y0 = f 0 y0
  where
  f x = if x > 10 * y0 then (x + _) else g x
    where
    g x' y' = f (x' + 10) (y' - 1)

ntco2 :: Int -> Int
ntco2 = f 0
  where
  f x y = if y <= 0 then x else g x (y - 1)
    where
    g x' = f (x' + 2)

ntco3 :: Int -> Int
ntco3 = f 0
  where
  f x y = if y <= 0 then x else g (y - 1)
    where
    g = f (x + 2)

ntco4 :: Int -> Int
ntco4 = f 0
  where
  f x y = if y <= 0 then x else g (y - 1)
    where
    g = h x
    h x' y' = f (x' + 2) y'

main :: Effect Unit
main = do
  assertEqual { expected: 200000, actual: tco1 100000 }
  assertEqual { expected: 200000, actual: tco2 100000 }
  assertEqual { expected: 249997, actual: tco3 100000 }
  assertEqual { expected: 200000, actual: tco4 100000 }

  assertEqual { expected: 1009, actual: ntco1 100 }
  assertThrows \_ -> ntco1 100000

  assertEqual { expected: 200, actual: ntco2 100 }
  assertThrows \_ -> ntco2 100000

  assertEqual { expected: 200, actual: ntco3 100 }
  assertThrows \_ -> ntco3 100000

  assertEqual { expected: 200, actual: ntco4 100 }
  assertThrows \_ -> ntco4 100000

  log "Done"
