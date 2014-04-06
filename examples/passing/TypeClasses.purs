module Main where

import Prelude

test1 = \_ -> show "testing"

f :: forall a. (Prelude.Show a) => a -> String
f x = show x

test2 = \_ -> f "testing"

test7 :: forall a. (Prelude.Show a) => a -> String
test7 = show

test8 = \_ -> show $ "testing"

data Data a = Data a

instance showData :: (Prelude.Show a) => Prelude.Show (Data a) where
  show (Data a) = "Data (" ++ show a ++ ")"

test3 = \_ -> show (Data "testing")

instance functorData :: Functor Data where
  (<$>) = liftM1

instance appData :: Applicative Data where
  pure = return
  (<*>) = ap

instance monadData :: Prelude.Monad Data where
  return = Data
  (>>=) (Data a) f = f a

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  (<$>) = liftM1

instance appMaybe :: Applicative Maybe where
  pure = return
  (<*>) = ap

instance monadMaybe :: Prelude.Monad Maybe where
  return = Just
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

test4 :: forall a m. (Monad m) => a -> m Number
test4 = \_ -> return 1

test5 = \_ -> Just 1 >>= \n -> return (n + 1)

instance showArray :: (Prelude.Show a) => Prelude.Show [a] where
  show [] = "[]"
  show (x:xs) = show x ++ ", " ++ show xs

test6 = \_ -> show ["testing"]

instance functorFunction :: Functor ((->) r) where
  (<$>) = liftM1

instance appFunction :: Applicative ((->) r) where
  pure = return
  (<*>) = ap

instance monadFunction :: Prelude.Monad ((->) r) where
  return a r = a
  (>>=) f g r = g (f r) r

ask r = r

runReader r f = f r

test9 _ = runReader 0 $ do
  n <- ask
  return $ n + 1
    
main = Debug.Trace.trace (test7 "Done")

