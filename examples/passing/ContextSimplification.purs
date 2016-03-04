module Main where

import Prelude

-- Here, we should simplify the context so that only one Eq
-- constraint is added.
usesEqTwice x = if x == x then x == x else false

main = Control.Monad.Eff.Console.log "Done"
