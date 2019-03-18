-- @shouldWarnWith WildcardInferredType
module Main where

x :: _
x = 42

y :: Int
y = 42 :: _

z :: Int
z = n
  where
  n :: _
  n = 42