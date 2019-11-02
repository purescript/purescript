-- @shouldFailWith TypesDoNotUnify
-- @shouldFailWith TypesDoNotUnify
module MultipleErrors where

import Prelude

foo :: Int -> Int
foo 0 = "Test"
foo n = bar (n - 1)

bar :: Int -> Int
bar 0 = "Test"
bar n = foo (n - 1)
