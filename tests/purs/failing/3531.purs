-- @shouldFailWith NoInstanceFound
module Main where

data Proxy a = Proxy

class C a where
  c :: Proxy a -> Boolean

instance c1 :: C String where
  c _ = true
else instance c2 :: C a where
  c _ = false

fn :: forall a. Proxy a -> Int
fn _ = 42 where
  x = c (Proxy :: Proxy a)
