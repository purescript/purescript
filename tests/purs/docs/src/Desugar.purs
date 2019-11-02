module Desugar where

data X a b = X a b

test :: forall a b. X (a -> b) a -> b
test x =
  let X a b = x
  in a b
