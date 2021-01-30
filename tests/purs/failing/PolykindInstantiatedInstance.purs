-- @shouldFailWith KindsDoNotUnify
module Main where

data Proxy a = Proxy

class F f where
  f :: forall a b. (a -> b) -> f a -> f b

instance fProxy :: F Proxy where
  f _ _ = Proxy

test1 = f (\a -> "foo") (Proxy :: _ "foo")
