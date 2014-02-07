module TypeSynonyms2 where

class Foo a where
  foo :: a -> String

type Bar = String

module M where

import TypeSynonyms2

instance TypeSynonyms2.Foo Bar where
  foo s = s
