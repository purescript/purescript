module Test where

test =
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  in
    foo

test = do
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  foo

test = ado
  let
    foo
      | bar
      , baz =
        42
      | otherwise = 100
  foo
