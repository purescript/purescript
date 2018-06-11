module PrimSubmodules (Lol(..), x, y, module O) where

import Prim.Ordering (kind Ordering, LT, EQ, GT) as O

data Lol (a :: O.Ordering) = Lol Int

x :: Lol O.LT
x = Lol 0

y :: Lol O.EQ
y = Lol 1
