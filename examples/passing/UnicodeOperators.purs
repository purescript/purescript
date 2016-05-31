module Main where

import Control.Monad.Eff.Console (log)

compose :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose f g a = f (g a)

infixr 9 compose as ∘

test1 = (\x -> x) ∘ \y -> y

elem :: forall a b. a -> (a -> Boolean) -> Boolean
elem x f = f x

infixl 1 elem as ∈

emptySet :: forall a. a -> Boolean
emptySet _ = true

test2 = 1 ∈ emptySet

main = log "Done"
