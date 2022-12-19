-- @shouldFailWith TypesDoNotUnify
module A where

-- Some labels are missing, these should be pointed out in the error 
f :: { a :: { c :: { d :: Int } }, d :: Int, e :: Int, f :: Int} -> Int
f { a: { b: { z } }, d, e, f, g } = z
