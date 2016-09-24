module Lib where

type Nat f g = ∀ x. f x → g x

infixr 4 type Nat as ~>

natId ∷ ∀ f. f ~> f
natId x = x
