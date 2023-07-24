-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class Multi tyNotAppearInBody norThisOne where
  useMulti :: Int

instance Multi Int Int where useMulti = 0

multi :: Int
multi = useMulti

