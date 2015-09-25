module Main where

import Prelude

class Su a where
  su :: a -> a

class (Su a) <= Cl a where
  cl :: a -> a -> a

instance suNumber :: Su Number where
  su n = n + 1.0

instance clNumber :: Cl Number where
  cl n m = n + m

test :: forall a. (Cl a) => a -> a
test a = su (cl a a)

main = Control.Monad.Eff.Console.print $ test 10.0
