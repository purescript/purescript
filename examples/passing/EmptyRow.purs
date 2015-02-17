module Main where

data Foo r = Foo { | r }

test :: Foo ()
test = Foo {}

main = Debug.Trace.trace "Done"
