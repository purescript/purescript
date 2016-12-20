-- @shouldFailWith KindsDoNotUnify
module Main where

-- Type-level strings have kind Prim.Symbol. If we define a kind Main.Symbol it
-- should be an error to use a type-level string were we expect a Main.Symbol.

foreign import kind Symbol

data SProxy (sym :: Symbol) = SProxy

example :: SProxy "Not that Symbol!"
example = SProxy

