-- @shouldFailWith HoleInferredType
module Main where

-- Expected:
--
-- aSaki/eSaki -> bNoa~cTowa -> dIbuki
--
-- Only aSaki/eSaki, bNoa, and cTowa is suggested.
--
-- The mutual recursion between bNoa and cTowa
-- ensures they exist "at the same time". dIbuki
-- depends on cTowa, so it's checked much later.

newtype K = K Int

aSaki :: Int -> K
aSaki = K

bNoa :: forall a. a -> K
bNoa a = let _ = cTowa a in K 0

cTowa :: forall a. a -> K
cTowa a = let _ = bNoa a in ?help 0

dIbuki :: Int -> K
dIbuki = bNoa

eSaki :: Int -> K
eSaki = K
