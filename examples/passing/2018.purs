module B where

  data Foo = X | Y

module A where

  import B as Main

  -- Prior to the 2018 fix this would be detected as a cycle between A and Main.
  foo ∷ Main.Foo → Main.Foo
  foo x = x

module Main where

import Prelude
import A (foo)
import B (Foo(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let tmp = foo X
  log "Done"

