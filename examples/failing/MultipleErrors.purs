module MultipleErrors where

import Prelude

foo :: Number -> Number
foo 0 = "Test"
foo n = bar (n - 1)

bar :: Number -> Number
bar 0 = "Test"
bar n = foo (n - 1)
