-- @shouldFailWith InvalidInstanceHead
module TypeWildcards where

import Prelude

data Foo a = Foo

instance showFoo :: Show (Foo _) where
  show Foo = "Foo"
