module Test where

test = do
  foo
  foo do
    bar
  <|> bar

