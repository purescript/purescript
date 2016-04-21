module Control.Semigroupoid where

-- | A `Semigroupoid` is similar to a [`Category`](#category) but does not
-- | require an identity element `id`, just composable morphisms.
-- |
-- | `Semigroupoid`s must satisfy the following law:
-- |
-- | - Associativity: `p <<< (q <<< r) = (p <<< q) <<< r`
-- |
-- | One example of a `Semigroupoid` is the function type constructor `(->)`,
-- | with `(<<<)` defined as function composition.
class Semigroupoid a where
  compose :: forall b c d. a c d -> a b c -> a b d

instance semigroupoidFn :: Semigroupoid (->) where
  compose f g x = f (g x)

infixr 9 compose as <<<

-- | Forwards composition, or `compose` with its arguments reversed.
composeFlipped :: forall a b c d. Semigroupoid a => a b c -> a c d -> a b d
composeFlipped f g = compose g f

infixr 9 composeFlipped as >>>
