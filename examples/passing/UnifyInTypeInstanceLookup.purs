module Main where

import Control.Monad.Eff.Console (log)

data T
data F

class EQ x y b
instance eqT :: EQ x x T
instance eqF :: EQ x y F

test :: forall a b. (EQ a b T) => a -> b -> a
test a _ = a

-- Note the expected type: 
--   Char -> Char 
-- Not 
--   forall t. (EQ t Char T) => t -> t
-- as in Haskell
test1 a = test a 'x'

main = log "Done"
