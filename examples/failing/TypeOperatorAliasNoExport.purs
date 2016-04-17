-- @shouldFailWith TransitiveExportError
module Test (type (×)) where

data Tuple a b = Tuple a b

infixl 6 type Tuple as ×
