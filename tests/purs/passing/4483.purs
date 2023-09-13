module Main where

import Effect.Console (log)
import Prim.TypeError

class Foo t where
  foo :: t -> String
  bar :: Int -> t

instance fooInt :: Fail (Text "can't use this") => Foo Int

main = log "Done"
