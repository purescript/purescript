module Main where

import Prelude

data Foo = Bar
type Baz = { | Foo }

main = Debug.Trace.trace "Done"
