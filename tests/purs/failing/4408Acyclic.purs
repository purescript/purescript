-- @shouldFailWith HoleInferredType
module Main where

-- Expected:
--
-- aRinku+cMuni -> bMaho -> dRei
--
-- Both aRinku and cMuni is suggested

newtype K = K Int

aRinku :: Int -> K
aRinku = K

bMaho :: K
bMaho = ?help 0

cMuni :: Int -> K
cMuni = K

dRei :: Int -> K
dRei _ = bMaho
