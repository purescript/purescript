module TypeWildcards where

data Foo a = Foo

instance showFoo :: Show (Foo _) where
  show Foo = "Foo"

