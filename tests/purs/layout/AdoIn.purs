module Test where

test = ado
  baz
  let foo = bar
   in bar

test = ado in foo

test = ado
  foo <- bar $ let a = 42 in a
  baz <- b
  in bar

test = ado
  foo
  let bar = let a = 42 in a
  let baz = 42
  in bar
