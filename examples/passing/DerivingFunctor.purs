module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert

data M f a = M0 a (Array a) | M1 Int | M2 (f a)

derive instance eqM :: (Eq (f a), Eq a) => Eq (M f a)

derive instance functorM :: Functor f => Functor (M f)

type MA = M Array

main = do
  assert $ map show (M0 0 [1, 2] :: MA Int) == M0 "0" ["1", "2"]
  assert $ map show (M1 0 :: MA Int) == M1 0
  assert $ map show (M2 [0, 1] :: MA Int) == M2 ["0", "1"]
  log "Done"
