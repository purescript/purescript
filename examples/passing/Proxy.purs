module Main (main) where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)

f :: @Int -> Unit
f _ = unit

g :: forall eff. @(console :: CONSOLE | eff) -> Unit
g _ = unit

h :: @"foo" -> Unit
h _ = unit

i :: @"foo"
i = @"foo"

j :: Unit
j = h i

main = log "Done"
