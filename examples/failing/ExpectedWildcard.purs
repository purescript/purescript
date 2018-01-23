-- @shouldFailWith ExpectedWildcard
module ExpectedWildcard where

import Data.Newtype

data Test = Test String

derive instance newtypeTest :: Newtype Test String
