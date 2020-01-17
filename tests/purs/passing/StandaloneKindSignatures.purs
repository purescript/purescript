module Main where

import Effect.Console (log)

data Pair :: forall k. k -> k -> Type
data Pair a b = Pair

newtype Pair' :: forall k. k -> k -> Type
newtype Pair' a b = Pair' (Pair a b)

type Fst :: forall k. k -> k -> k
type Fst a b = a

class To :: forall k. k -> k -> Constraint
class To a b | a -> b

test1 = Pair :: Pair Int String
test2 = Pair :: Pair "foo" "bar"
test3 = Pair' Pair :: Pair' Int String
test4 = Pair' Pair :: Pair' "foo" "bar"
test5 = 42 :: Fst Int String
test6 = Pair :: Pair (Fst "foo" "bar") "baz"

instance to1 :: To Int String
instance to2 :: To "foo" "bar"

main = log "Done"
