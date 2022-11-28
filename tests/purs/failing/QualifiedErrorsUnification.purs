-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Prelude (Unit)
import Data.List.Lazy (nil)
import Data.List.Types (List)

wrongModule :: List Unit
wrongModule = nil
