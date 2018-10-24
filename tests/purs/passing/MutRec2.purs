module Main where

import Prelude
import Effect.Console (log)

data A = A B

data B = B A

foreign import data S :: Type

f :: A -> S
f a = case a of A b -> g b

g b = case b of B a -> f a

showN :: A -> S
showN a = f a

main = log "Done"
