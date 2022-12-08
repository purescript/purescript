-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude

-- Test that the inferred type doesn't leak the local type synonym in the
-- produced error message.

intentionallyUntyped = wrap thing
  where
  type Wrapped a = { wrapped :: a }
  wrap :: forall a. a -> Wrapped a
  wrap wrapped = { wrapped }
  thing :: Int
  thing = 0

foo :: Int
foo = if intentionallyUntyped == 0 then 0 else 1
