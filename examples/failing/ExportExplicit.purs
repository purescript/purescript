-- @shouldFailWith UnknownExport
-- should fail as z does not exist in the module
module M1 (x, y, z) where

import Prelude

x = 1
y = 2
