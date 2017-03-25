module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assert)

class Foo a

instance foo1 :: Foo Number

instance foo2 :: Foo Number

test :: forall a. Foo a => a -> a
test a = a

test1 = test 0.0

main = do
  assert (test1 == 0.0)
  log "Done"
