module Main where

import Prelude
import Control.Monad.Eff.Console (log)

data Either a b = Left a | Right b

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right (f y)

main = log "Done"
