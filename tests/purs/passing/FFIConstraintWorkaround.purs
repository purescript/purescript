module Main where

import Prelude
import Effect
import Effect.Console
import Test.Assert

main :: Effect Unit
main = do
  assert' "Showing Int is correct" $ showFFI 4 == "4"
  assert' "Showing String is correct" $ showFFI "string" == "\"string\""
  assert' "Showing Record is correct" $
    showFFI { a: 1, b: true, c: 'd', e: 4.0 } == "{ a: 1, b: true, c: 'd', e: 4.0 }"
  log "Done"

showFFI :: forall a. Show a => a -> String
showFFI = showImpl show

-- Since type class constraints are not allowed
-- in FFI declarations, we have to pass members
-- we want to use into the function itself.
foreign import showImpl :: forall a. (a -> String) -> a -> String
