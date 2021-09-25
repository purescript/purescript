module Main where

import Effect.Console (log)
import ImportedClassName as I

data Foo = Foo

class ClassName a where
  foo :: a -> Int

instance ClassName Foo where
  foo _ = 0
instance I.ClassName Foo where
  foo _ = 0

main = log "Done"
