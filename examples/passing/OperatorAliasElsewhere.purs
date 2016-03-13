module Def where

what :: forall a b. a -> b -> a
what a _ = a

module Main where

import Prelude
import Def (what)
import Control.Monad.Eff.Console

infixl 4 what as ?!

main = log $ "Done" ?! true
