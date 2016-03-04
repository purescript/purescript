module Main where

import Prelude
import Control.Monad.Eff.Console

shout = log <<< (<> "!") <<< show

-- Here, we should simplify the context so that only one Show
-- constraint is added.
usesShowTwice true = shout
usesShowTwice false = print

main = usesShowTwice true "Done"
