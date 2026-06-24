-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

-- Test that the inferred type doesn't leak and unify with the shadowed type
-- with the same name, and also that the error message doesn't include the
-- local type synonym.

data Wrapped a = Wrapped a

intentionallyUntyped = wrap thing
  where
  type Wrapped a = { wrapped :: a }
  wrap :: forall a. a -> Wrapped a
  wrap wrapped = { wrapped }
  thing :: Int
  thing = 0

unwrap :: Wrapped Int -> Int
unwrap (Wrapped w) = w

foo :: Int
foo = unwrap intentionallyUntyped
