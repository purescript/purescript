module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Foo t where
  foo :: forall a. a -> t

instance fooUnit :: Foo Unit where
  foo :: forall a. a -> Unit
  foo = go
    where
      go :: a -> Unit
      go = \_ -> unit

instance fooNumber :: Foo Int where
  foo :: forall b. b -> Int
  foo = go
    where
      go :: b -> Int
      go = \_ -> 0

main = log "Done"
