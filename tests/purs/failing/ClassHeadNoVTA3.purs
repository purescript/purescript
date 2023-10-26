-- @shouldFailWith NoInstanceFound
module Main where

class MultiMissing tyNotAppearInBody norThisOne where
  useMultiMissing :: Int

multiMissing :: Int
multiMissing = useMultiMissing @Int

