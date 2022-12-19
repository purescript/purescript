-- @shouldFailWith PropertyIsMissing
module A where

-- Currently only reports on the missing `c` label
doStuff :: { a :: Int, b :: Int, c :: Int, d :: Int } -> { a :: Int, b :: Int } -> Int
doStuff q z = doStuff z q
