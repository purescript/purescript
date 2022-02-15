module Main where

import Effect.Console

data Tuple a b = Tuple a b

tuple :: forall @a @b. a -> b -> Tuple a b
tuple = Tuple

tuple' :: Tuple Int Int
tuple' = tuple @Int @Int 21 42

data Id a = Id a

id :: forall @a. a -> Id a
id = Id

id' :: Id (forall a. a -> a)
id' = id @(forall a. a -> a) (\x -> x)

main = log "Done"
