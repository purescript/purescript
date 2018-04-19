-- @shouldFailWith CannotDerive
module DeriveOldGeneric where

import Prelude
import Data.Generic
import Effect.Console (log)

newtype Foo = Foo Int

derive instance genericFoo :: Generic Foo
