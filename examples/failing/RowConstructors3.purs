module Main where

type Foo = { x :: Number }
type Bar = { | Foo }

main = Debug.Trace.trace "Done"
