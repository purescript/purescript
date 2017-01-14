module Main where

import Prelude
import Control.Monad.Eff.Console

type T = { foo :: Int, bar :: { baz :: Int, qux :: Int } }

init :: T
init = { foo: 1, bar: { baz: 2, qux: 3 } }

updated :: T
updated = init { foo = 10, bar.baz = 20, bar.qux = 30 }

expected :: T
expected = { foo: 10, bar: { baz: 20, qux: 30 } }

check l r =
  l.foo == r.foo &&
  l.bar.baz == r.bar.baz &&
  l.bar.qux == r.bar.qux

main = do
  when (check updated expected) $ log "Done"
