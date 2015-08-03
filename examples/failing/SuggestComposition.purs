-- @shouldFailWith TypesDoNotUnify
-- TODO: Ensure the correct suggestion is produced.
module SuggestComposition where

import Prelude

f = g . g where g = (+1)
