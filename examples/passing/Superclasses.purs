module Main where

class Su a where
  su :: a -> a

class (Su a) <= Cl a where
  cl :: a -> a -> a

test :: forall a. (Cl a) => a -> a
test a = su (cl a a)

main = Debug.Trace.trace "Done"
