-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
module Main where

x :: Int
x = 0 :: _

y :: _
y = 0

z :: Int
z =
  let n :: _
      n = 0
  in n

w :: Int
w = n
  where
  n :: _
  n = 0
