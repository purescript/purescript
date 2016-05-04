-- @shouldFailWith TransitiveExportError
-- exporting `a` should fail as `A` is hidden
module Foo (B(..), a, b) where

data A = A
data B = B

a = A
b = B
