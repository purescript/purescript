-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class Multi tyNotAppearInBody norThisOne where
  useMulti :: Int

multi :: Int
multi = useMulti

