module Main where

import Data.Array
import Data.Maybe
import Effect.Console (log)

class Foo a where
  foo :: a

class Bar a where
  bar :: a

data Simple = Simple

instance fooArray1 :: forall a. Foo a => Foo (Array a) where
  foo :: Array a
  foo = []

instance fooArray2 :: forall a. Foo (Maybe a) where
  foo :: Maybe a
  foo = Nothing

instance forall a. Bar a => Bar (Array a) where
  bar :: Array a
  bar = []

instance forall a. Bar (Maybe a) where
  bar :: Maybe a
  bar = Nothing

main = log "Done"
