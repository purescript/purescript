module Main where

import Prelude
import Control.Monad.Eff.Console (log)

newtype F r a = F { x :: a | r }

unF :: forall r a. F r a -> { x :: a | r }
unF (F x) = x

derive instance functorF :: Functor (F r)

main = log (unF (map id (F { x: "Done", y: 42 }))).x
