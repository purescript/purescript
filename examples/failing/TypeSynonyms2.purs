module TypeSynonyms2 where

class Foo a where
  foo :: a -> String

type Bar = String

instance Foo Bar where
  foo s = s
