-- @shouldFailWith QuantificationCheckFailureInInstance

module FooMod where

import Data.Maybe

class Foo a where
  foo :: a

class Bar a where
  bar :: a

class Baz a where
  baz :: a

-- Unknown type 'a'
instance fooMaybe :: forall b. Foo (Maybe a) where
  foo :: Maybe a
  foo = Nothing

-- Unknown type 'b' in contraint
instance barMaybe :: forall a. Bar b => Bar (Maybe a) where
  bar :: Maybe a
  bar = Nothing

-- Unknown type 'b' in instance body
instance bazMaybe :: forall a. Baz (Maybe a) where
  baz :: Maybe b
  baz = Nothing