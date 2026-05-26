-- @shouldFailWith OverlappingInstances
module Main where

import Prelude
import Data.Newtype (class Newtype, unwrap)

newtype Wrapper = Wrapper String
  derive (Newtype)

derive instance Newtype Wrapper _

value :: String
value = unwrap (Wrapper "hi")
