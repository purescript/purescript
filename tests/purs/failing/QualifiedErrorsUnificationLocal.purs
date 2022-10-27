-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Prelude (Unit)
import Data.List.Lazy (nil)

data List (a :: Type)

wrongModule :: List Unit
wrongModule = nil
