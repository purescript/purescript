module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)

data Foo = X | Y

cond ∷ ∀ a. Boolean → a → a → a
cond = if _ then _ else _

what ∷ Boolean → Foo
what = if _ then X else Y

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let tmp1 = what true
      tmp2 = cond true 0 1
  log "Done"
