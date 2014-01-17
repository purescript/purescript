module TypeClasses where

class Show a where
  show :: a -> String

instance TypeClasses.Show String where
  show s = s

test1 = show "testing"

f :: forall a. (TypeClasses.Show a) => a -> String
f x = show x

test2 = f "testing"

data Data a = Data a

instance (TypeClasses.Show a) => TypeClasses.Show (Data a) where
  show (Data a) = "Data (" ++ show a ++ ")"

test3 = show (Data "testing")

class Monad m where
  ret :: forall a. a -> m a
  bind :: forall a b. m a -> (a -> m b) -> m b

instance TypeClasses.Monad Data where
  ret = Data
  bind (Data a) f = f a
