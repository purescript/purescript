-- @shouldFailWith InvalidOperatorInBinder
module Main where

data List a = Cons a (List a) | Nil

cons ∷ ∀ a. a → List a → List a
cons = Cons

infixl 6 cons as :

get ∷ ∀ a. List a → a
get (_ : x : _) = x
