-- @shouldFailWith NoInstanceFound
module X where

data E a b = L a | R b

g :: forall a b . E a b -> a
g e | L x <- e = x
