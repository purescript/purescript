-- @shouldFailWith NoInstanceFound
module Main where

class MultiFd tyNotAppearInBody norThisOne | tyNotAppearInBody -> norThisOne where
  useMultiFd :: Int

multiFd :: Int
multiFd = useMultiFd
