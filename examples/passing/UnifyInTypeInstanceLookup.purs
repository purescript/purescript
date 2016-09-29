module Main where

import Control.Monad.Eff.Console (log)

data Z = Z
data S n = S n

data T
data F

class EQ x y b
instance eqT :: EQ x x T
instance eqF :: EQ x y F

test :: forall a b. (EQ a b T) => a -> b -> a
test a _ = a

spin :: forall a b. a -> b
spin a = spin a

-- Expected type: 
-- forall t. (EQ t (S Z) T) => t
test1 = test (spin 1) (S Z)

main = log "Done"
