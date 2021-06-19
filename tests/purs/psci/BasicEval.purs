import Prelude
import Data.Array

-- @shouldEvaluateTo 3628800
let fac n = foldl mul 1 (1..n) in fac 10

fac n = foldl mul 1 (1..n)

-- @shouldEvaluateTo 3628800
fac 10

infix 4 mul as |*|

-- @shouldEvaluateTo 50
5 |*| 10

data X a = X

type role X representational
