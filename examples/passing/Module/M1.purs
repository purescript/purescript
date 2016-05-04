module M1 where

import Prelude

data Foo = Foo String

foo :: M1.Foo -> String
foo = \f -> case f of Foo s -> s <> "foo"

bar :: Foo -> String
bar = foo

incr :: Int -> Int
incr x = x + 1
