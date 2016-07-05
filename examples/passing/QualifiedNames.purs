module Main where

import Prelude
import Either as Either
import Control.Monad.Eff.Console (log)

either :: forall a b c. (a -> c) -> (b -> c) -> Either.Either a b -> c
either f _ (Either.Left x) = f x
either _ g (Either.Right y) = g y

main = log (either id id (Either.Left "Done"))
