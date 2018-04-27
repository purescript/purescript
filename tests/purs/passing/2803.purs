module Main where

import Prelude ((+), (-), (==))
import Effect.Console (log)

f :: Int -> Int -> Int
f = (+)

infixl 6 f as %

g :: Int -> Int -> Int
g a b = let f = (-) in a % b

main =
  if g 10 5 == 15
    then log "Done"
    else log "Failed"
