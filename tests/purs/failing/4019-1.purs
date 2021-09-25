-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

newtype Indexed ∷ forall k1 k2 k3. (k1 → Type) → k2 → k3 → k1 → Type
newtype Indexed m x y a = Indexed (m a)

class IxFunctor ∷ ∀ ix. (ix → ix → Type → Type) → Constraint
class IxFunctor f where
  imap ∷ ∀ a b x y. (a → b) → f x y a → f x y b

instance ixFunctorIndexed ∷ Functor m ⇒ IxFunctor (Indexed m) where
  imap f (Indexed ma) = Indexed (map f ma)

foreign import data K1 :: Type
foreign import data K2 :: Type

foreign import data D1 :: K1
foreign import data D2 :: K2

foo :: Indexed Array D1 D2 Int
foo = Indexed [1]

bar :: Indexed Array D1 D2 Int
bar = imap identity foo
