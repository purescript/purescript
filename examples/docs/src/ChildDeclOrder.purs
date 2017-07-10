-- Tests should ensure that, in the docs:
--  - First should come before Second
--  - foo1 should be listed before foo2
--  - the instances should be listed in the same order as this source file
module ChildDeclOrder where

data Two
  = First
  | Second

class Show a where
  show :: a -> String

class Foo a where
  foo1 :: a
  foo2 :: a

instance showTwo :: Show Two where
  show _  = ""

instance fooTwo :: Foo Two where
  foo1 = First
  foo2 = Second

instance fooInt :: Foo Int where
  foo1 = 1
  foo2 = 2
