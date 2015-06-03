module Main where

import Prelude
import Prelude.Unsafe (unsafeIndex)

class Su a where
  su :: a -> a

class (Su (Array a)) <= Cl a where
  cl :: a -> a -> a

instance suNumber :: Su Number where
  su n = n + 1.0

instance suArray :: (Su a) => Su (Array a) where
  su [x] = [su x]

instance clNumber :: Cl Number where
  cl n m = n + m

test :: forall a. (Cl a) => a -> Array a
test x = su [cl x x]

main = Debug.Trace.print $ test 10.0 `unsafeIndex` 0.0
