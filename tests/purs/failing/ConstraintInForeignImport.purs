-- @shouldFailWith ErrorParsingModule
module Main where

import Data.Show (class Show)

foreign import show :: ∀ a. Show a => a -> String
