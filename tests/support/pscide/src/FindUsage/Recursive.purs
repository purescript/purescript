module FindUsage.Recursive where

data Nat = Suc Nat | Z

recursiveUsage :: Nat -> Int
recursiveUsage = case _ of
  Suc x -> recursiveUsage x
  Z -> 0
