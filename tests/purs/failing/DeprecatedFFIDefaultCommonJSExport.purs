-- @shouldFailWith DeprecatedFFIDefaultCommonJSExport
module Main where

foreign import default :: forall a. a
