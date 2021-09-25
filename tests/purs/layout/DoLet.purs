module Test where

test = do
  let foo = bar
  foo

test = do
  let foo = bar
  in baz
  foo

test = do
  let foo = bar
    in baz
  foo
