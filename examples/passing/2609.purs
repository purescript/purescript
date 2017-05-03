module Main where

import Prelude
import Eg (Foo'(Bar'), (:->))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

bar' :: Foo'
bar' = 4 :-> 5

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = case bar' of Bar' l r -> log "Done"
