module Main where

import Prelude
import Control.Monad.Eff.Console (log)

testTopLevel :: _ -> _
testTopLevel n = n + 1.0

test :: forall a. Eq a => (a -> a) -> a -> a
test f a = go (f a) a
  where
  go :: _ -> _ -> _
  go a1 a2 | a1 == a2 = a1
  go a1 _ = go (f a1) a1

main = log "Done"
