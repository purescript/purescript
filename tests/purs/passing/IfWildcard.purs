module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

data Foo = X | Y

cond ∷ ∀ a. Boolean → a → a → a
cond = if _ then _ else _

what ∷ Boolean → Foo
what = if _ then X else Y

main :: Effect Unit
main = do
  let tmp1 = what true
      tmp2 = cond true 0 1
  log "Done"
