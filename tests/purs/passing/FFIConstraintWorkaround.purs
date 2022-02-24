module Main where

import Prelude
import Effect
import Effect.Console

main :: Effect Unit
main = do
  log "Done"

showFFI :: forall a. Show a => a -> String
showFFI = showImpl show

-- Since type class constraints are not allowed
-- in FFI declarations, we have to pass members
-- we want to use into the function itself.
foreign import showImpl :: forall a. (a -> String) -> a -> String
