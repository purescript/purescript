import Prelude
import Data.Array

-- @paste
fac :: Int -> Int
fac n = foldl mul 1 (1..n)
-- @paste

-- @shouldEvaluateTo 3628800
fac 10
