module FindUsage.RecursiveShadowed where

data Nat = Suc Nat | Z

recursiveUsage :: Nat -> Int
recursiveUsage = case _ of
  Suc x ->
    let recursiveUsage = 3
    in recursiveUsage
  Z -> 0
