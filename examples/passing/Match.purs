module Main where

import Prelude

data Foo a = Foo

foo = \f -> case f of Foo -> "foo"

main = Debug.Trace.trace "Done"
