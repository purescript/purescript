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

instance applyData :: Apply Data where
  (<*>) = ap

instance applicativeData :: Applicative Data where
  pure = return

instance bindData :: Bind Data where
  (>>=) (Data a) f = f a

instance monadData :: Monad Data where
  return = Data

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  (<$>) = liftM1

instance applyMaybe :: Apply Maybe where
  (<*>) = ap

instance applicativeMaybe :: Applicative Maybe where
  pure = return

instance bindMaybe :: Bind Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

instance monadMaybe :: Monad Maybe where
  return = Just

test4 :: forall a m. (Monad m) => a -> m Number
test4 = \_ -> return 1

test5 = \_ -> Just 1 >>= \n -> return (n + 1)

instance showArray :: (Prelude.Show a) => Prelude.Show [a] where
  show [] = "[]"
  show (x:xs) = show x ++ ", " ++ show xs

test6 = \_ -> show ["testing"]

instance functorFunction :: Functor ((->) r) where
  (<$>) = liftM1

instance applyFunction :: Apply ((->) r) where
  (<*>) = ap

instance applicativeFunction :: Applicative ((->) r) where
  pure = return

instance bindFunction :: Bind ((->) r) where
  (>>=) f g r = g (f r) r

instance monadFunction :: Monad ((->) r) where
  return a r = a

ask r = r

runReader r f = f r

test9 _ = runReader 0 $ do
  n <- ask
  return $ n + 1

main = Debug.Trace.trace (test7 "Done")

