-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
module Main where

x :: Int
x = 0 :: ?x

y :: ?y
y = 0

z :: Int
z =
  let n :: ?n
      n = 0
  in n

w :: Int
w = n
  where
  n :: ?n
  n = 0

v :: ?v
v = 0
