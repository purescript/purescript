module UnknownSuperclassTypeVar where

class Foo a

class (Foo b) <= Bar a
