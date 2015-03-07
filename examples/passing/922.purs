module Main where

class Foo a where
  foo :: Number -> a

instance fooNumber :: Foo Number where
  foo = id

bar :: forall a. (Foo a) => a
bar = foo 1

main =
  case bar of
    1 -> Debug.Trace.trace "Done"
