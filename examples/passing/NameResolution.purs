foo = \x -> x

module A where
    bar = foo

    module B where

        abc = foo
        xyz = bar

    foo = \x y -> x

    baz = foo

    abc = A.B.abc
