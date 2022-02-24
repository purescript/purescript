module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log $ showFFI 4
  log $ showFFI "string"
  log $ showFFI { a: 1, b: true, c: 'd', e: 4.0 }

showFFI :: forall a. Show a => a -> String
showFFI = showImpl show

-- Since type class constraints are not allowed
-- in FFI declarations, we have to pass members
-- we want to use into the function itself.
foreign import showImpl :: forall a. (a -> String) -> a -> String
