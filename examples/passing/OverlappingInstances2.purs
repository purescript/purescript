module Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Test.Assert (assert)

data A = A | B

instance eqA1 :: Eq A where
  eq A A = true
  eq B B = true
  eq _ _ = false

instance eqA2 :: Eq A where
  eq _ _ = true

instance ordA :: Ord A where
  compare A B = LT
  compare B A = GT
  compare _ _ = EQ

test :: forall a. Ord a => a -> a -> String
test x y = show $ x == y

main = do
  assert $ test A B == "false"
  log "Done"
