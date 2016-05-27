module Main where

import Prelude
import Def (what)
import Control.Monad.Eff.Console

infixl 4 what as ?!

main = log $ "Done" ?! true
