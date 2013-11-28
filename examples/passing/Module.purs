module Test where

  data Foo = Foo String

  foo :: Test:Foo -> String
  foo = \f -> case f of Foo s -> s ++ "foo"

  bar :: Foo -> String
  bar = foo

  module Inner where

    inner = "Inner"

baz :: Test:Foo -> String
baz = Test:foo

innerCopy :: String
innerCopy = Test:Inner:inner

match :: Test:Foo -> String
match = \f -> case f of Test:Foo s -> s ++ "foo"
