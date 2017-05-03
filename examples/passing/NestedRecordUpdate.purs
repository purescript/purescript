module Main where

import Prelude
import Control.Monad.Eff.Console

type T = { foo :: Int, bar :: { baz :: Int, qux :: { lhs :: Int, rhs :: Int } } }

init :: T
init = { foo: 1, bar: { baz: 2, qux: { lhs: 3, rhs: 4 } } }

updated :: T
updated = init { foo = 10, bar { baz = 20, qux { lhs = 30, rhs = 40 } } }

expected :: T
expected = { foo: 10, bar: { baz: 20, qux: { lhs: 30, rhs: 40 } } }

check l r =
  l.foo == r.foo &&
  l.bar.baz == r.bar.baz &&
  l.bar.qux.lhs == r.bar.qux.lhs &&
  l.bar.qux.rhs == r.bar.qux.rhs

main = do
  when (check updated expected) $ log "Done"
