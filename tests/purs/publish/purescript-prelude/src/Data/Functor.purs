module Data.Functor
  ( class Functor, map, (<$>)
  , mapFlipped, (<#>)
  , void
  , voidRight, (<$)
  , voidLeft, ($>)
  , flap, (<@>)
  ) where

import Data.Function (const, compose)
import Data.Unit (Unit, unit)

-- | A `Functor` is a type constructor which supports a mapping operation
-- | `map`.
-- |
-- | `map` can be used to turn functions `a -> b` into functions
-- | `f a -> f b` whose argument and return types use the type constructor `f`
-- | to represent some computational context.
-- |
-- | Instances must satisfy the following laws:
-- |
-- | - Identity: `map identity = identity`
-- | - Composition: `map (f <<< g) = map f <<< map g`
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

-- | `mapFlipped` is `map` with its arguments reversed. For example:
-- |
-- | ```purescript
-- | [1, 2, 3] <#> \n -> n * n
-- | ```
mapFlipped :: forall f a b. Functor f => f a -> (a -> b) -> f b
mapFlipped fa f = f <$> fa

infixl 1 mapFlipped as <#>

instance functorFn :: Functor ((->) r) where
  map = compose

instance functorArray :: Functor Array where
  map = arrayMap

foreign import arrayMap :: forall a b. (a -> b) -> Array a -> Array b

-- | The `void` function is used to ignore the type wrapped by a
-- | [`Functor`](#functor), replacing it with `Unit` and keeping only the type
-- | information provided by the type constructor itself.
-- |
-- | `void` is often useful when using `do` notation to change the return type
-- | of a monadic computation:
-- |
-- | ```purescript
-- | main = forE 1 10 \n -> void do
-- |   print n
-- |   print (n * n)
-- | ```
void :: forall f a. Functor f => f a -> f Unit
void = map (const unit)

-- | Ignore the return value of a computation, using the specified return value
-- | instead.
voidRight :: forall f a b. Functor f => a -> f b -> f a
voidRight x = map (const x)

infixl 4 voidRight as <$

-- | A version of `voidRight` with its arguments flipped.
voidLeft :: forall f a b. Functor f => f a -> b -> f b
voidLeft f x = const x <$> f

infixl 4 voidLeft as $>

-- | Apply a value in a computational context to a value in no context.
-- |
-- | Generalizes `flip`.
-- |
-- | ```purescript
-- | longEnough :: String -> Bool
-- | hasSymbol :: String -> Bool
-- | hasDigit :: String -> Bool
-- | password :: String
-- |
-- | validate :: String -> Array Bool
-- | validate = flap [longEnough, hasSymbol, hasDigit]
-- | ```
-- |
-- | ```purescript
-- | flap (-) 3 4 == 1
-- | threeve <$> Just 1 <@> 'a' <*> Just true == Just (threeve 1 'a' true)
-- | ```
flap :: forall f a b. Functor f => f (a -> b) -> a -> f b
flap ff x = map (\f -> f x) ff

infixl 4 flap as <@>
