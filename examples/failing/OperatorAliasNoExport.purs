-- @shouldFailWith TransitiveExportError
module Test ((?!)) where

infixl 4 what as ?!

what :: forall a b. a -> b -> a
what a _ = a
