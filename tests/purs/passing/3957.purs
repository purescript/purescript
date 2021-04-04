module Main where

import Prelude
import Effect.Console (log)
import Test.Assert (assertEqual)

data Maybe a = Nothing | Just a

f :: Int -> Int
f x = case x of
  0 -> 0
  n | _ <- n -> f (x - 1)
  _ -> f (x - 2)

g :: Int -> Int
g x = case x of
  0 -> 0
  n | n == n, true -> g (x - 1)
  _ -> g (x - 2)

weirdsum :: Int -> (Int -> Maybe Int) -> Int -> Int
weirdsum accum f n = case n of
  0 -> accum
  x | Just y <- f x -> weirdsum (accum + y) f (n - 1)
  _ -> weirdsum accum f (n - 1)

tricksyinners :: Int -> Int -> Int
tricksyinners accum x = case x of
  0 -> accum + f' x * f' x
  n -> tricksyinners (accum + 2) (n - 1)
  where
  f' y = y + 3

main = do
  assertEqual { expected: 0, actual: f 100000 }
  assertEqual { expected: 0, actual: g 100000 }
  assertEqual { expected: 20, actual: weirdsum 0 (\x -> if x < 5 then Just (2 * x) else Nothing) 100000 }
  assertEqual { expected: 200009, actual: tricksyinners 0 100000 }
  log "Done"
