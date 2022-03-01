-- @shouldFailWith DeprecatedFFICommonJSModule
module Main where

import Effect.Console (log)

foreign import default :: String

main = log default
