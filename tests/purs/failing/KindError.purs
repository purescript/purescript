-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

data KindError f a = One f | Two (f a)
