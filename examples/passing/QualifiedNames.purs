module Main where

import Prelude

either :: forall a b c. (a -> c) -> (b -> c) -> Either.Either a b -> c
either f _ (Either.Left x) = f x
either _ g (Either.Right y) = g y

main = Control.Monad.Eff.Console.log (either id id (Either.Left "Done"))
