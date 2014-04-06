module CycleInSuperclasses where

class (Foo a) <= Bar a

class (Bar a) <= Foo a

instance barString :: Bar String

instance fooString :: Foo String
