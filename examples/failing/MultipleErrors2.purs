-- @shouldFailWith UnknownName
-- @shouldFailWith UnknownName
module MultipleErrors2 where

import Prelude

foo = itDoesntExist

bar = neitherDoesThis
