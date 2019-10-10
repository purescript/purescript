-- @shouldFailWith CycleInDeclaration
-- Example submitted by dbeacham in issue #3407.
module Main where

import Prelude
import Data.Maybe

newtype MyRec = MyRec { a :: Int, b :: Maybe MyRec }

derive newtype instance showMyRec :: Show MyRec
