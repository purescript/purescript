module Main where

instance fooNumber :: Foo Number where
  foo = 0

class Foo a where
  foo :: a

main = Debug.Trace.trace "Done"
