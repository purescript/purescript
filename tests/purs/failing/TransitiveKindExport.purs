-- @shouldFailWith TransitiveExportError
module Main (TestProxy(..)) where

foreign import kind Test

data TestProxy (p :: Test) = TestProxy
