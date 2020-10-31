-- @shouldFailWith NoInstanceFound
module Main where

import Safe.Coerce (coerce)

data F a = F a
type Synonym a = F a
data Representational a = Representational (Synonym a)

representationalToRepresentational :: Representational Int -> Representational String
representationalToRepresentational = coerce
