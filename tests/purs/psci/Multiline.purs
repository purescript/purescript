-- @paste
import Prelude
import Data.Array
-- @paste

-- @paste
fac :: Int -> Int
fac n = foldl mul 1 (1..n)
-- @paste

-- @shouldEvaluateTo 3628800
fac 10

-- @paste
data X :: Type -> Type
data X a = X
-- @paste
