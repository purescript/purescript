module Main where

import Prelude.Unsafe (unsafeIndex)

class Su a where
  su :: a -> a

class (Su [a]) <= Cl a where
  cl :: a -> a -> a

instance suNumber :: Su Number where
  su n = n + 1

instance suArray :: (Su a) => Su [a] where
  su (x : _) = [su x]

instance clNumber :: Cl Number where
  cl n m = n + m

test :: forall a. (Cl a) => a -> [a]
test x = su [cl x x]

main = Debug.Trace.print $ test 10 `unsafeIndex` 0
