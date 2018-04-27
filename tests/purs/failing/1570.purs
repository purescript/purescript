-- @shouldFailWith ExpectedType
module M where

data F a = F a

test = \(x :: F) -> x
