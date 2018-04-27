-- @shouldFailWith UndefinedTypeVariable
module UnknownSuperclassTypeVar where

import Prelude

class Foo a

class (Foo b) <= Bar a
