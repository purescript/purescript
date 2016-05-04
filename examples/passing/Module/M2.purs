module M2 where

import Prelude

baz :: M1.Foo -> String
baz = M1.foo

match :: M1.Foo -> String
match = \f -> case f of M1.Foo s -> s <> "foo"
