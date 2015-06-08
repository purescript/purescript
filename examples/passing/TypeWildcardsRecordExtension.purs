module Main where

import Prelude

foo :: forall a. {b :: Number | a} -> {b :: Number | _}
foo f = f

main = Debug.Trace.trace "Done"
