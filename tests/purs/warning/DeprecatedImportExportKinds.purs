-- @shouldWarnWith WarningParsingModule
-- @shouldWarnWith WarningParsingModule
-- @shouldWarnWith WarningParsingModule
module Main where

import Lib (kind Foo)

foreign import data Bar :: Foo
