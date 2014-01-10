module M1 where

data Test = Test    

module M2 where

import M1

test :: Test -> M1.Test
test t = t

testCtor = Test
