-- @shouldFailWith NoInstanceFound
module Main where

import Prelude

class Single tyNotAppearInBody where
  useSingle :: Int

instance Single Int where useSingle = 0

single :: Int
single = useSingle
