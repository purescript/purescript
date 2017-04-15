module Main (main) where

import Prelude
import Control.Monad.Eff.Console (CONSOLE, log)

f :: proxy Int -> Unit
f _ = unit

g :: forall eff. proxy (console :: CONSOLE | eff) -> Unit
g _ = unit

h :: proxy "foo" -> Unit
h _ = unit

i :: proxy "foo"
i = @"foo"

j :: Unit
j = h i

main = log "Done"
