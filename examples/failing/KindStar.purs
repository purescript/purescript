-- @shouldFailWith ExpectedType

module X where

data List a = Nil | Cons a (List a)

test :: List
test = Nil
