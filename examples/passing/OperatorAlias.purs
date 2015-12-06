module Main where

import Prelude
import Control.Monad.Eff.Console

infixl 4 what as ?!

what :: forall a b. a -> b -> a
what a _ = a

main = log $ "Done" ?! true
