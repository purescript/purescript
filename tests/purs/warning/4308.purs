-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
-- @shouldWarnWith WildcardInferredType
module Main where

-- No warnings expected here because `f` has full type signature
f :: Int
f = (\(y :: _) -> (y :: _)) 42

-- All three warnings expected here because the type signature of `g` has a
-- wildcard in it. One warning for the top-level signature wildcard, one for the
-- wildcard in the lambda parameter pattern, and one in the lambda body.
g :: _
g = (\(y :: _) -> (y :: _)) 42
