-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class MultiMissing tyNotAppearInBody norThisOne where
  useMultiMissing :: Int

instance MultiMissing Int Int where useMultiMissing = 0
instance MultiMissing Int String where useMultiMissing = 1

multiMissing :: Int
multiMissing = useMultiMissing @Int

class MultiFd tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne where
  useMultiFd :: Int

instance MultiFd Int Int where useMultiFd = 0

multiFd :: Int
multiFd = useMultiFd

class MultiFdBidi tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne, norThisOne -> tyNotAppearInBody  where
  useMultiFdBidi :: Int

instance MultiFdBidi Int Int where useMultiFdBidi = 0

multiFdBidi :: Int
multiFdBidi = useMultiFdBidi
