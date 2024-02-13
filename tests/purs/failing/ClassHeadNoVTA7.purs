-- @shouldFailWith NoInstanceFound
module ClassHeadNoVTA7 where

import Prelude

import Data.Maybe (Maybe(..))

class TestClass a b | a -> b, b -> a where
  testMethod :: Maybe a -> Int

test :: Int
test = testMethod Nothing
