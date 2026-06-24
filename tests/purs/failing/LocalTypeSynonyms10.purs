-- @shouldFailWith KindsDoNotUnify
module Main where

data Proxy a = Proxy

test :: Int
test = 0
  where
  type A :: Proxy -- this should fail, not the actual synonym declaration
  type A = "nope"
