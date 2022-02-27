module Test where

example1 = do
  foo bar
  <|> baz

example2 = do
  foo bar
  `wat` baz

example3 =
  case _ of
    Foo a -> 1
    Bar b -> 2
    `append` 3

example4 =
  case _ of
    Foo a -> 1
    Bar b -> 2
    + 3
