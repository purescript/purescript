module FooMod where

import Data.Maybe

class Foo a where
  foo :: a

class Bar a where
  bar :: a

-- type var 'a' not used
instance forall a. Foo (Maybe Int) where
  foo :: Maybe Int
  foo = Nothing

-- type var 'b' not used
instance forall a b. (Bar a, Bar b) => Bar (Maybe a) where
  bar :: Maybe a
  bar = Nothing
