module Main where

import Prelude

type Foo = { x :: Number }
type Bar = { | Foo }

main = Debug.Trace.trace "Done"
