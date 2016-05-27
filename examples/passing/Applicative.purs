module Main where

import Control.Monad.Eff.Console (log)

class Applicative f where
  pure :: forall a. a -> f a
  apply :: forall a b. f (a -> b) -> f a -> f b

data Maybe a = Nothing | Just a

instance applicativeMaybe :: Applicative Maybe where
  pure = Just
  apply (Just f) (Just a) = Just (f a)
  apply _ _ = Nothing

main = log "Done"
