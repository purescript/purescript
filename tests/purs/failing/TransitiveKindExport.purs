-- @shouldFailWith TransitiveExportError
module Main (TestProxy(..)) where

data Test

data TestProxy (p :: Test) = TestProxy
