-- @shouldFailWith NoInstanceFound
module Main where

class MultiFdBidi tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne, norThisOne -> tyNotAppearInBody where
  useMultiFdBidi :: Int

multiFdBidi :: Int
multiFdBidi = useMultiFdBidi
