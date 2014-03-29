module Main where

class Foo a where
  foo :: a -> String

instance fooString :: Foo String where
  foo = go
    where
    go :: String -> String
    go s = s

main = Debug.Trace.trace "Done"
