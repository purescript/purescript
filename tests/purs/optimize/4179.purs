module Main where

import Prelude

import Partial.Unsafe (unsafePartial)

isEven n = n == 0 || isOdd (n - 1)
isOdd n = n > 0 && not (isEven (n - 1))

-- This is an example of four mutually recursive bindings with a complex
-- run-time dependency structure. The expected result is:
--   alpha is defined without any laziness
--   bravo and charlie are lazily initialized in a group
--   and then delta is lazily initialized

alpha :: Int -> Int -> Number
alpha = case _ of
  0 -> bravo
  1 -> charlie
  2 -> \y -> if y > 0 then bravo y else charlie y
  x -> \y -> delta y x

-- Me: `alpha`
-- purs: The value of alpha is undefined here, so this reference is not allowed.
-- Me: `(\_ -> alpha) {}`
-- purs: LGTM!

bravo :: Int -> Number
bravo = (\_ -> alpha) {} 3

charlie :: Int -> Number
charlie = (\_ -> alpha) {} 4

delta :: Int -> Int -> Number
delta =
  let b = (\_ -> bravo) {}
  in \x y -> if x == y then b 0 else 1.0


-- This is a test that TCO isn't broken by unsafePartial.

tcoable :: Int -> String
tcoable = unsafePartial case _ of
  0 -> "done"
  n | n > 0 -> tcoable (n - 1)
