module Main where

import Prelude

import Control.Monad.Eff.Console (log)

runFn3 :: forall a b c d. (a -> b -> c -> d) -> a -> b -> c -> d
runFn3 f a b c = f a b c

main = do
  log $ runFn3 (\a b c -> c) 1 2 "Done"
