-- @shouldFailWith UnknownExportDataConstructor
-- should fail as Y is not a data constructor for X
module M1 (X(Y)) where

import Prelude

data X = X
data Y = Y
