module TypeClassInstances where

class A a where
  a :: a -> String
  b :: a -> Number

instance A String where
  a s = s
