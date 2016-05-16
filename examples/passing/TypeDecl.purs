module Main where

import Prelude
import Control.Monad.Eff.Console (log)

k :: String -> Number -> String
k x y = x

iterate :: forall a. Number -> (a -> a) -> a -> a
iterate 0.0 f a = a
iterate n f a = iterate (n - 1.0) f (f a)

main = log "Done"
