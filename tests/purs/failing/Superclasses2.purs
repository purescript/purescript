-- @shouldFailWith CycleInTypeSynonym
-- TODO: Should this have its own error, perhaps CycleInTypeClassDeclaration?
module CycleInSuperclasses where

import Prelude

class (Foo a) <= Bar a

class (Bar a) <= Foo a

instance barString :: Bar String

instance fooString :: Foo String
