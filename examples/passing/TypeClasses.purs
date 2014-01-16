module TypeClasses where

class Show a where
  show :: a -> String

instance TypeClasses.Show String where
  show s = s

test1 = show "testing"

f :: forall a. (TypeClasses.Show a) => a -> String
f x = show x

test2 = f "testing"
