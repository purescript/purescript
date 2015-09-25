module Main where

import Prelude
import Control.Monad.Eff

type Foo a = forall f. (Monad f) => f a

foo :: forall a. a -> Foo a
foo x = pure x

bar :: Foo Number
bar = foo 3.0

main = do
  x <- bar
  Control.Monad.Eff.Console.print x
