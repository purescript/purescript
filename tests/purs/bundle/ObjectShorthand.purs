-- See issue #3741
module Main (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

main :: Effect Unit
main = do
  assert' "bar" (bar.foo == 1)
  assert' "quux" (quux 3 == { baz: 3 })
  assert' "baz" bazIsEliminated
  log "Done"

foreign import bar :: { foo :: Int }
foreign import quux :: forall a. a -> { baz :: a }
foreign import bazIsEliminated :: Boolean
