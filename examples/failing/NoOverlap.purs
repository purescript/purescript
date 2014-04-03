module Main where

data Foo = Foo

instance showFoo1 :: Show Foo where
  show _ = "Foo"

instance showFoo2 :: Show Foo where
  show _ = "Bar"

test = show Foo
