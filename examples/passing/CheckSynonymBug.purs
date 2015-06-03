module Main where

import Prelude

type Foo a = Array a

foo _ = length ([] :: Foo Number)

main = Debug.Trace.trace "Done"
