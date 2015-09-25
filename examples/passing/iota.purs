module Main where

s = \x -> \y -> \z -> x z (y z)

k = \x -> \y -> x

iota = \x -> x s k

main = Control.Monad.Eff.Console.log "Done"
