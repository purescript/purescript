module Main where

import Prelude
import Effect.Console (log)

class Foo a where
  foo :: a -> String

instance fooString :: Foo String where
  foo s = s

main = log $ foo "Done"
