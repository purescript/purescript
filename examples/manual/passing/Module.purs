module M1 where

  import Prelude

  data Foo = Foo String

  foo :: M1.Foo -> String
  foo = \f -> case f of Foo s -> s ++ "foo"

  bar :: Foo -> String
  bar = foo

  incr :: Number -> Number
  incr x = x + 1

module M2 where

  import Prelude

  baz :: M1.Foo -> String
  baz = M1.foo

  match :: M1.Foo -> String
  match = \f -> case f of M1.Foo s -> s ++ "foo"

module Main where

main = Control.Monad.Eff.Console.log "Done"
