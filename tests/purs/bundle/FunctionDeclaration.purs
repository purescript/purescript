module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

main :: Effect Unit
main = do
  assert' "foo" fooIsEliminated
  qux >>= log

foreign import qux :: Effect String
foreign import fooIsEliminated :: Boolean
