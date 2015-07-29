module Main where

import Prelude

data Either a b = Left a | Right b

instance functorEither :: Prelude.Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right (f y)

main = Control.Monad.Eff.Console.log "Done"
