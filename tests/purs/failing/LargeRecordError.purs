-- @shouldFailWith TypesDoNotUnify
module A where

-- Some labels are missing, these should be pointed out in the error 
f :: { a :: Int, b :: Int, c :: Int, d :: Int, e :: Int, f :: Int, h :: Int, j :: Int } -> Int
f { a, b, c, d, e, f, g } = 1
