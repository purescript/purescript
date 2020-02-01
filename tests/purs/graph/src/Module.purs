module Module (foo) where

import Module2 (bar)

foo :: Int
foo = 0

baz :: Int
baz = foo + bar
