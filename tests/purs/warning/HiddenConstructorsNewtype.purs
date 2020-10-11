-- @shouldWarnWith HiddenConstructors
module Main (N) where

import Data.Newtype (class Newtype)

newtype N a = N a

derive instance newtypeN :: Newtype (N a) _
