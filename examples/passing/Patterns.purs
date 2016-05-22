module Main where

import Prelude
import Control.Monad.Eff.Console (log)

test = \x -> case x of
  { str: "Foo", bool: true } -> true
  { str: "Bar", bool: b } -> b
  _ -> false

f = \o -> case o of
  { foo: "Foo" } -> o.bar
  _ -> 0

h = \o -> case o of
  a@[_,_,_] -> a
  _ -> []

isDesc :: Array Number -> Boolean
isDesc [x, y] | x > y = true
isDesc _ = false

main = log "Done"
