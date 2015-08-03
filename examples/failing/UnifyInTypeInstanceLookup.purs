-- @shouldFailWith NoInstanceFound
-- See issue #390.
-- TODO: Improve this error.
module Main where

import Prelude

data Z = Z
data S n = S n

data T
data F

class EQ x y b
instance eqT :: EQ x x T
instance eqF :: EQ x y F

foreign import test :: forall a b. (EQ a b T) => a -> b -> a

foreign import anyNat :: forall a. a

test1 = test anyNat (S Z)
