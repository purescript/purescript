module Main where

import Prelude
import Control.Monad.Eff.Console

id' = (\x -> x) <$> \y -> y

main = log (id' "Done")
