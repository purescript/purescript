module Main where

foo :: forall a. {b :: Number | a} -> {b :: Number | _}
foo f = f

main = Debug.Trace.trace "Done"
