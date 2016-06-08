module List where

data List a = Cons a (List a) | Nil

infixr 6 Cons as :
