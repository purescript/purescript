-- @shouldFailWith NoInstanceFound

module Main where

import Prelude
import Prim.TypeError
import Effect (Effect)
import Effect.Console (log)

class MyShow a where
  myShow :: a -> String

instance cannotShowFunctions :: Fail (Text "Cannot show functions") => MyShow (a -> b) where
  myShow _ = "unreachable"

main :: Effect Unit
main = log (myShow (_ + 1))
