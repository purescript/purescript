-- @shouldFailWith KindsDoNotUnify
module LocalTypeSynonyms4 where

wrap :: forall a. a -> { wrapped :: a }
wrap a = result
  where
  type T r = { wrapped :: a | r }
  result :: T a
  result = { wrapped: a }
