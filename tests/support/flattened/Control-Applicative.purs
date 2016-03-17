module Control.Applicative
  ( class Applicative, pure
  , liftA1
  , unless, when
  , module Control.Apply
  , module Data.Functor
  ) where

import Control.Apply (class Apply, apply, (*>), (<*), (<*>))

import Data.Functor (class Functor, map, void, ($>), (<#>), (<$), (<$>))
import Data.Unit (Unit, unit)

-- | The `Applicative` type class extends the [`Apply`](#apply) type class
-- | with a `pure` function, which can be used to create values of type `f a`
-- | from values of type `a`.
-- |
-- | Where [`Apply`](#apply) provides the ability to lift functions of two or
-- | more arguments to functions whose arguments are wrapped using `f`, and
-- | [`Functor`](#functor) provides the ability to lift functions of one
-- | argument, `pure` can be seen as the function which lifts functions of
-- | _zero_ arguments. That is, `Applicative` functors support a lifting
-- | operation for any number of function arguments.
-- |
-- | Instances must satisfy the following laws in addition to the `Apply`
-- | laws:
-- |
-- | - Identity: `(pure id) <*> v = v`
-- | - Composition: `(pure <<<) <*> f <*> g <*> h = f <*> (g <*> h)`
-- | - Homomorphism: `(pure f) <*> (pure x) = pure (f x)`
-- | - Interchange: `u <*> (pure y) = (pure ($ y)) <*> u`
class Apply f <= Applicative f where
  pure :: forall a. a -> f a

instance applicativeFn :: Applicative ((->) r) where
  pure x _ = x

instance applicativeArray :: Applicative Array where
  pure x = [x]

-- | `liftA1` provides a default implementation of `(<$>)` for any
-- | [`Applicative`](#applicative) functor, without using `(<$>)` as provided
-- | by the [`Functor`](#functor)-[`Applicative`](#applicative) superclass
-- | relationship.
-- |
-- | `liftA1` can therefore be used to write [`Functor`](#functor) instances
-- | as follows:
-- |
-- | ```purescript
-- | instance functorF :: Functor F where
-- |   map = liftA1
-- | ```
liftA1 :: forall f a b. Applicative f => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

-- | Perform a applicative action when a condition is true.
when :: forall m. Applicative m => Boolean -> m Unit -> m Unit
when true m = m
when false _ = pure unit

-- | Perform a applicative action unless a condition is true.
unless :: forall m. Applicative m => Boolean -> m Unit -> m Unit
unless false m = m
unless true _ = pure unit
