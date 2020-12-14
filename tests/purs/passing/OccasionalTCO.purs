module Main where

import Prelude
import Effect.Console (log)

main = do
  let _ = occasionalTCO1 10000000
  let _ = occasionalTCO2 10000000
  let _ = occasionalTCO3 10000000
  let _ = occasionalTCO4 10000000
  let _ = occasionalTCO5 10000000
  log "Done"

occasionalTCO1 :: Int -> Int
occasionalTCO1 0 = 1
occasionalTCO1 n =
  occasionalTCO1 (n - occasionalTCO1 0)

occasionalTCO2 :: Int -> Int
occasionalTCO2 0 = 1
occasionalTCO2 n =
  let x = occasionalTCO2 0
  in occasionalTCO2 (n - x)

occasionalTCO3 :: Int -> Int
occasionalTCO3 0 = 1
occasionalTCO3 n =
  if occasionalTCO3 0 == n
  then 1
  else occasionalTCO3 (n - occasionalTCO3 0)

occasionalTCO4 :: Int -> Int
occasionalTCO4 0 = 1
occasionalTCO4 n | 1 <- occasionalTCO4 0 =
  case occasionalTCO4 0 + n of
    2 -> 1
    x -> occasionalTCO4 (x - 2)
occasionalTCO4 _ = 1

occasionalTCO5 :: Int -> Int
occasionalTCO5 0 = 1
occasionalTCO5 n | n > 10 =
  occasionalTCO5 (n - 1)
occasionalTCO5 n =
  if n > 5
  then occasionalTCO5 $ n - 1
  else call occasionalTCO5 (n - 1)

call f x = f x
