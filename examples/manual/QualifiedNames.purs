module Data.Either where

import Prelude

data Either a b = Left a | Right b

module Main where

either :: forall a b c. (a -> c) -> (b -> c) -> Data.Either.Either a b -> c
either f _ (Data.Either.Left x) = f x
either _ g (Data.Either.Right y) = g y

main = Control.Monad.Eff.Console.log (either id id (Data.Either.Left "Done"))
