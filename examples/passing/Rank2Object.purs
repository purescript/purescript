module Main where

import Prelude
import Debug.Trace

data Foo = Foo { id :: forall a. a -> a }

foo :: Foo -> Number
foo (Foo { id = f }) = f 0.0

main = trace "Done"
