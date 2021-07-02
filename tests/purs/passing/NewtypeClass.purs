module Main where

import Prelude
import Effect
import Effect.Console
import Safe.Coerce (class Coercible, coerce)

class Newtype :: Type -> Type -> Constraint
class Coercible t a <= Newtype t a | t -> a

wrap :: forall t a. Newtype t a => a -> t
wrap = coerce

unwrap :: forall t a. Newtype t a => t -> a
unwrap = coerce

instance newtypeMultiplicative :: Newtype (Multiplicative a) a

newtype Multiplicative a = Multiplicative a

instance semiringMultiplicative :: Semiring a => Semigroup (Multiplicative a) where
  append (Multiplicative a) (Multiplicative b) = Multiplicative (a * b)

data Pair a = Pair a a

foldPair :: forall a s. Semigroup s => (a -> s) -> Pair a -> s
foldPair f (Pair a b) = f a <> f b

ala
  :: forall f t a
   . Functor f
  => Newtype t a
  => (a -> t)
  -> ((a -> t) -> f t)
  -> f a
ala _ f = map unwrap (f wrap)

test = ala Multiplicative foldPair

test1 = ala Multiplicative foldPair (Pair 2 3)

main = do
  logShow (test (Pair 2 3))
  log "Done"
