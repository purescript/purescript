-- @shouldFailWith ExtraneousClassMember
module Main where

import Prelude

class A a where
  a :: a -> String

instance aString :: A String where
  a s = s
  b x = x
