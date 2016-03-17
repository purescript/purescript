module Control.Category
  ( class Category, id
  , module Control.Semigroupoid
  ) where

import Control.Semigroupoid (class Semigroupoid, compose, (<<<), (>>>))

-- | `Category`s consist of objects and composable morphisms between them, and
-- | as such are [`Semigroupoids`](#semigroupoid), but unlike `semigroupoids`
-- | must have an identity element.
-- |
-- | Instances must satisfy the following law in addition to the
-- | `Semigroupoid` law:
-- |
-- | - Identity: `id <<< p = p <<< id = p`
class Semigroupoid a <= Category a where
  id :: forall t. a t t

instance categoryFn :: Category (->) where
  id x = x
