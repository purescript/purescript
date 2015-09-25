-- @shouldFailWith UndefinedTypeVariable
module UnknownSuperclassTypeVar where

import Prelude

class Foo a where
  foo :: a -> a

class (Foo b) <= Bar a
