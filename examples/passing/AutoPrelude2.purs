module Main where

import Prelude
import Prelude as P
import Control.Monad.Eff.Console

f :: forall a. a -> a
f = P.id

main = P.($) log ((f P.<<< f) "Done")
