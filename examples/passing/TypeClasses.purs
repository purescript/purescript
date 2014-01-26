module TypeClasses where

class Show a where
  show :: a -> String

instance Show String where
  show s = s

test1 = \ -> show "testing"

f :: forall a. (TypeClasses.Show a) => a -> String
f x = show x

test2 = \ -> f "testing"

data Data a = Data a

instance (TypeClasses.Show a) => TypeClasses.Show (Data a) where
  show (Data a) = "Data (" ++ show a ++ ")"

test3 = \ -> show (Data "testing")

class Monad m where
  ret :: forall a. a -> m a
  (>>=) :: forall a b. m a -> (a -> m b) -> m b

instance TypeClasses.Monad Data where
  ret = Data
  (>>=) (Data a) f = f a

data Maybe a = Nothing | Just a

instance TypeClasses.Monad Maybe where
  ret = Just
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

test4 :: forall m. (Monad m) => () -> m Number
test4 = \ -> ret 1

test5 = \ -> Just 1 >>= \n -> ret (n + 1)

module TypeClasses2 where

import TypeClasses

instance (TypeClasses.Show a) => TypeClasses.Show [a] where
  show [] = "[]"
  show (x:xs) = TypeClasses.show x ++ ", " ++ TypeClasses.show xs

test6 = \ -> show ["testing"]
    
module Main where

main = Trace.trace "Done"

