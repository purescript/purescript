module Main where

import Effect.Console (log)

s = \x -> \y -> \z -> x z (y z)

k = \x -> \y -> x

iota = \x -> x s k

main = log "Done"
