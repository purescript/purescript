-- @shouldFailWith NoInstanceFound
module Main where

data Proxy a = Proxy

class C a b where
  c :: Proxy a -> Proxy b -> Boolean

instance c1 :: C String String where
  c _ _ = true
else instance c2 :: C String a where
  c _ _ = false

instance c3 :: C Int Int where
  c _ _ = true
else instance c4 :: C Int a where
  c _ _ = false

fn :: forall a b. Proxy a -> Proxy b -> Int
fn _ _ = 42 where
  x = c (Proxy :: Proxy a) (Proxy :: Proxy b)
