import Prelude
import Data.Array

-- @shouldEvaluateTo 3628800
let fac n = foldl mul 1 (1..n) in fac 10

fac n = foldl mul 1 (1..n)

-- @shouldEvaluateTo 3628800
fac 10
