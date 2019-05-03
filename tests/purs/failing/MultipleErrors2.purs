-- @shouldFailWith UnknownValueHint
-- @shouldFailWith UnknownValueHint
module MultipleErrors2 where

import Prelude

foo = itDoesntExist

bar = neitherDoesThis
