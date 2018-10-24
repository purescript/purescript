module VendoredVariant where

import Prelude

import Prim.Row as Row

import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafeCrashWith)
import Data.Symbol

data FProxy (k :: Type -> Type) = FProxy
data VariantF (f :: # Type) a

newtype VariantFRep f a = VariantFRep
  { type :: String
  , value :: f a
  , map :: forall x y. (x -> y) -> f x -> f y
  }

case_ :: forall a b. VariantF () a -> b
case_ r = unsafeCrashWith case unsafeCoerce r of
    VariantFRep v -> "failure on " <> v.type

on
  :: forall sym f a b r1 r2
  . Row.Cons sym (FProxy f) r1 r2
  => IsSymbol sym
  => SProxy sym
  -> (f a -> b)
  -> (VariantF r1 a -> b)
  -> VariantF r2 a
  -> b
on p f g r =
  case coerceY r of
    VariantFRep v | v.type == reflectSymbol p -> f v.value
    _ -> g (coerceR r)
  where
  coerceY :: VariantF r2 a -> VariantFRep f a
  coerceY = unsafeCoerce

  coerceR :: VariantF r2 a -> VariantF r1 a
  coerceR = unsafeCoerce
