-- @shouldFailWith TypesDoNotUnify
-- TODO: Check that this does not produce a "function composition is (<<<)"
-- suggestion.
module DoNotSuggestComposition where

import Prelude

x = { y: 3 }

foo :: String -> String
foo y = y

bar = foo x
