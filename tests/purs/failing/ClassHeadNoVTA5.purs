-- @shouldFailWith NoInstanceFound
module Main where

-- Verify that args in output match order defined here:
-- `tyNotAppearInBody` appears before `norThisOne`
class MultiFdBidi tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne, norThisOne -> tyNotAppearInBody where
  useMultiFdBidi :: Int

multiFdBidi :: Int
multiFdBidi = useMultiFdBidi
