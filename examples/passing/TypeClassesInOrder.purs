module Main where

import Prelude

class Foo a where
  foo :: a -> String

instance Foo String where
  foo s = s

main = Debug.Trace.trace $ foo "Done"
