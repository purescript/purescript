module B (module A, A(..)) where

import A

-- | Test that there's no name collision between the imported module `A` and the
-- | data constructor `A`.
data A = A
