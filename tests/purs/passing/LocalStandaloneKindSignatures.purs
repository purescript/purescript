module Main where

import Effect.Console (log)

data Pair :: forall k. k -> k -> Type
data Pair a b = Pair

data Proxy a = Proxy

foo :: forall k (s :: k) (t :: k). Proxy s -> Proxy t -> Proxy k
foo = result
  where
  type Fst :: forall k. k -> k -> k
  type Fst a b = a

  test1 = 42 :: Fst Int String
  test2 = Pair :: Pair (Fst "foo" "bar") "baz"

  type ConstS :: forall t. t -> k
  type ConstS a = s

  type Const :: forall k t. k -> t -> k
  type Const s a = s

  result :: Proxy (ConstS "foo") -> Proxy (Const t "bar") -> Proxy k
  result _ _ = Proxy


main = log "Done"
