-- @shouldWarnWith HiddenConstructors
module Main (D) where

import Data.Generic.Rep (class Generic)

data D = D

derive instance genericD :: Generic D _
