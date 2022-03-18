-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith MissingTypeDeclaration
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith MissingTypeDeclaration
module Main where

import Prelude

x :: _
x = 42

y :: Int
y = 42 :: _

z :: Int
z = n
  where
  n :: _
  n = 42

-- Inner signatures can suppress warnings from more-inner wildcards,
-- even though a top-level signature is missing (see #4268)

alpha = f 0
  where
  f :: Int -> Int
  f m = n
    where
    n :: _
    n = m + 1  

-- Tests for recursive binding groups (see #4268)

bravo :: Int -> Int
bravo m = if n > 0 then bravo (n - 1) else n
  where
  n :: _
  n = m

charlie :: Int -> Int
charlie m = if n > 0 then delta (n - 1) else n
  where
  n :: _
  n = m

delta m = if n > 0 then charlie (n - 1) else n
  where
  n = m

echo :: _ -> Int -- Partial signatures don't count!
echo m = if n > 0 then foxtrot (n - 1) else n
  where
  n :: _
  n = m

foxtrot :: Int -> Int
foxtrot m = if n > 0 then echo (n - 1) else n
  where
  n = m
