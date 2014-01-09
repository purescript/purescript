module TailCall where

test n [] = n
test n (x:xs) = test (n + x) xs

