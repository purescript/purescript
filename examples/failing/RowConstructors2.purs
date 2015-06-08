module Main where

import Prelude

type Foo r = (x :: Number | r)
type Bar = { | Foo }

main = Debug.Trace.trace "Done"
