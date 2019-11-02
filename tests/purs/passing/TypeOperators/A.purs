module A
( Tuple(..)
, type (/\)
, (/\)
, Natural
, type (~>)
) where

data Tuple a b = Tuple a b

infixl 6 Tuple as /\
infixl 6 type Tuple as /\

type Natural f g = ∀ a. f a → g a

infixr 0 type Natural as ~>

tup ∷ ∀ a b. a → b → b /\ a
tup a b = b /\ a

tupX ∷ ∀ a b c. a /\ b /\ c → c
tupX (a /\ b /\ c) = c
