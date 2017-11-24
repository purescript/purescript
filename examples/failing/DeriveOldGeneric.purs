-- @shouldFailWith CannotDerive
module DeriveOldGeneric where

import Prelude
import Data.Generic
import Control.Monad.Eff.Console (log)

newtype Foo = Foo Int

derive instance genericFoo :: Generic Foo
