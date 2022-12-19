-- @shouldFailWith TypesDoNotUnify
-- @shouldFailWith TypesDoNotUnify
module A where

-- Find both these errors
f :: { a :: String, b :: String } -> Int
f { a: 0, b: 0 } = 0
f { } = 0
