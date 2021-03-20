-- @shouldFailWith TypesDoNotUnify
module Main where

import Safe.Coerce (coerce)

recToRec :: { x :: Int } -> { y :: String }
recToRec = coerce
