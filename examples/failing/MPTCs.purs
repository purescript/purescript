-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

class Foo a where
  f :: a -> a

instance fooStringString :: Foo String String where
  f a = a
