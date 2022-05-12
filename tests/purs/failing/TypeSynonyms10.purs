-- @shouldFailWith KindsDoNotUnify
module Main where

import Prelude

type F (a :: Type) = a

newtype N = N (F ((~>) Array))
