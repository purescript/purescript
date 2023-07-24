-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class MultiFd tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne where
  useMultiFd :: Int

instance MultiFd Int Int where useMultiFd = 0

multiFd :: Int
multiFd = useMultiFd
