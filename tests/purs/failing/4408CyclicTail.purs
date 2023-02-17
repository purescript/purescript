-- @shouldFailWith HoleInferredType
module Main where

-- Expected:
--
-- aKyoko -> bShinobu~cEsora/eShinobu~fEsora -> dYuka
--
-- All are suggested, as dYuka is also recursive.

newtype K = K Int

aKyoko :: Int -> K
aKyoko = K

bShinobu :: forall a. a -> K
bShinobu a = let _ = cEsora a in K 0

cEsora :: forall a. a -> K
cEsora a = let _ = bShinobu a in K 0

dYuka :: Int -> K
dYuka _ = ?help 0

eShinobu :: forall a. a -> K
eShinobu a = let _ = fEsora a in K 0

fEsora :: forall a. a -> K
fEsora a = let _ = eShinobu a in K 0
