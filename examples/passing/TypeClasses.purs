module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test1 = \_ -> show "testing"

f :: forall a. (Show a) => a -> String
f x = show x

test2 = \_ -> f "testing"

test7 :: forall a. (Show a) => a -> String
test7 = show

test8 = \_ -> show $ "testing"

data Data a = Data a

instance showData :: (Show a) => Show (Data a) where
  show (Data a) = "Data (" <> show a <> ")"

test3 = \_ -> show (Data "testing")

instance functorData :: Functor Data where
  map = liftM1

instance applyData :: Apply Data where
  apply = ap

instance applicativeData :: Applicative Data where
  pure = Data

instance bindData :: Bind Data where
  bind (Data a) f = f a

instance monadData :: Monad Data

data Maybe a = Nothing | Just a

instance functorMaybe :: Functor Maybe where
  map = liftM1

instance applyMaybe :: Apply Maybe where
  apply = ap

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  bind Nothing _ = Nothing
  bind (Just a) f = f a

instance monadMaybe :: Monad Maybe

test4 :: forall a m. (Monad m) => a -> m Number
test4 = \_ -> pure 1.0

test5 = \_ -> Just 1.0 >>= \n -> pure (n + 1.0)

ask r = r

runReader r f = f r

test9 _ = runReader 0.0 $ do
  n <- ask
  pure $ n + 1.0

main = do
  log (test7 "Hello")
  log "Done"
