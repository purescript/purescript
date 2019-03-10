module Data.Symbol
  ( class IsSymbol
  , reflectSymbol
  , reifySymbol
  , SProxy(..)
  ) where

-- | A value-level proxy for a type-level symbol.
data SProxy (sym :: Symbol) = SProxy

-- | A class for known symbols
class IsSymbol (sym :: Symbol) where
  reflectSymbol :: SProxy sym -> String

-- local definition for use in `reifySymbol`
foreign import unsafeCoerce :: forall a b. a -> b

reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => SProxy sym -> r) -> r
reifySymbol s f = coerce f { reflectSymbol: \_ -> s } SProxy where
  coerce
    :: (forall sym1. IsSymbol sym1              => SProxy sym1 -> r)
    -> { reflectSymbol :: SProxy "" -> String } -> SProxy ""   -> r
  coerce = unsafeCoerce

