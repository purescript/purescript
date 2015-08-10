module Main where

import Prelude

data A = A B

data B = B A

foreign import data S :: *

f :: A -> S
f a = case a of A b -> g b

g b = case b of B a -> f a

showN :: A -> S
showN a = f a

main = Control.Monad.Eff.Console.log "Done"
