module Main where

import Prelude
import Data.Generic
import Control.Monad.Eff.Console (log)

type Foo = { foo :: Int }

newtype Foo' = Foo' Foo

derive instance genericFoo :: Generic Foo'

main = log "Done"
