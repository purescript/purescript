-- @shouldFailWith MissingFFIImplementations
module Main where

foreign import yes :: Boolean
foreign import no :: Boolean
