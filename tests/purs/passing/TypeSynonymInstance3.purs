module Main where

import Effect.Console (log)

data Cons a b
infix 6 type Cons as :*

data D2
data D5
data D6
data D8

type D256 = D2 :* (D5 :* D6)

class LtEq a b

instance ltEqD8D256 :: LtEq D8 D256

class (LtEq a D256) <= Lte256 a

instance lte256 :: Lte256 D8

main = log "Done"
