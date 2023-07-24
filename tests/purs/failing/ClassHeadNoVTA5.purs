-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class MultiFdBidi tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne, norThisOne -> tyNotAppearInBody  where
  useMultiFdBidi :: Int

instance MultiFdBidi Int Int where useMultiFdBidi = 0

multiFdBidi :: Int
multiFdBidi = useMultiFdBidi
