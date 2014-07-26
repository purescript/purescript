module Main where

class OverlappingVars a where
  f :: a -> a

data Foo a b = Foo a b

instance overlappingVarsFoo :: OverlappingVars (Foo a a) where
  f a = a

test = f (Foo "" 0)

