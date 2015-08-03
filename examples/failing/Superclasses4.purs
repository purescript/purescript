-- @shouldFailWith OverlappingInstances
module OverlappingInstances where

import Prelude

class Foo a

instance foo1 :: Foo Number

instance foo2 :: Foo Number

test :: forall a. (Foo a) => a -> a
test a = a

test1 = test 0.0
