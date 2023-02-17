-- @shouldFailWith HoleInferredType
module Main where

-- Expected:
--
-- aHaruna/eHaruna -> bMiyu~cKurumi~dMiiko
--
-- All are suggested.

newtype K = K Int

aHaruna :: Int -> K
aHaruna = K

bMiyu :: forall a. a -> K
bMiyu a = let _ = dMiiko a in K 0

cKurumi :: forall a. a -> K
cKurumi a = let _ = bMiyu a in K 0

dMiiko :: forall a. a -> K
dMiiko a = let _ = cKurumi a in ?help 0

eHaruna :: Int -> K
eHaruna = K
