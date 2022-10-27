-- @shouldFailWith TypesDoNotUnify
module Main where

import Prelude (Unit)
import Data.List.Lazy (nil)
import Data.List (List)

wrongModule :: List Unit
wrongModule = nil
