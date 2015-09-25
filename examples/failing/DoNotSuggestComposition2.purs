-- @shouldFailWith CannotApplyFunction
-- TODO: Check that this does not produce a "function composition is (<<<)"
-- suggestion.

module DoNotSuggestComposition2 where

foo = let x = { y: 3 } in x 2
