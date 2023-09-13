-- @shouldFailWith HoleInferredType
module Main where

-- Expected:
--
-- aRinku+cMuni -> bMaho -> dRei
--
-- aRinku, cMuni, and bMaho are all suggested.
-- bMaho can be aware of itself during checking.

newtype K = K Int

aRinku :: Int -> K
aRinku = K

bMaho :: Int -> K
bMaho _ = ?help 0

cMuni :: Int -> K
cMuni = K

dRei :: Int -> K
dRei _ = bMaho
