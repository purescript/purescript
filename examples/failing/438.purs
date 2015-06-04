module Main where

import Prelude

data Fix f = In (f (Fix f))

instance eqFix :: (Eq (f (Fix f))) => Eq (Fix f) where
  (==) (In f) (In g) = f == g
  (/=) a b = not (a == b)

example = In [] == In []
