module Main where

import Prelude
import Control.Monad.Eff.Console (log)

class Foo a where
  foo ∷ a → a

test ∷ ∀ a. (Foo a) ⇒ a → a
test = foo

main = log "Done"
