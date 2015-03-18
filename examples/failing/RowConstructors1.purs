module Main where

data Foo = Bar
type Baz = { | Foo }

main = Debug.Trace.trace "Done"
