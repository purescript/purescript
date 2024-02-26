-- @shouldFailWith TypesDoNotUnify
module A where

f :: { a :: Int, b :: Int, c :: Int, d :: Int }  -> Int
f r = f r

problems :: Int
problems = f { a: 1, b: 2, y: 9, z: 9 }
