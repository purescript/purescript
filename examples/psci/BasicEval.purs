import Prelude
import Data.Array

let fac n = foldl mul 1 (1..n) in fac 10 -- 3628800

fac n = foldl mul 1 (1..n)
fac 10 -- 3628800
