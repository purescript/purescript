-- @shouldFailWith TypesDoNotUnify
-- @shouldFailWith TypesDoNotUnify
module A where

-- Find both these errors
f :: { a :: String, b :: Boolean } -> Char
f { a: 0, b: 0 } = 'a'
f {} = 'b'
