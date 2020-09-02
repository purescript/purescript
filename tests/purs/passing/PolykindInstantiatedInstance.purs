module Main where

import Effect.Console (log)

data Proxy a = Proxy

class F f where
  f :: forall a b. (a -> b) -> f a -> f b

instance fProxy :: F Proxy where
  f _ _ = Proxy

test1 :: forall a. Proxy a
test1 = f (\a -> a) Proxy

test2 :: Proxy Int
test2 = f (\a -> a) (Proxy :: Proxy Int)

test3 :: Proxy String
test3 = f (\a -> "foo") Proxy

main = log "Done"
