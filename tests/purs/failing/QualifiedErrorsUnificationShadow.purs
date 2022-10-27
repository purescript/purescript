-- @shouldFailWith TypeConstructorsDoNotUnify
module Main where

import Prelude (Unit)
import Data.List.Lazy (nil)
import Data.List.Types (List) as Data.List.Lazy.Types

wrongModule :: Data.List.Lazy.Types.List Unit
wrongModule = nil
