module Main where

import Prelude
import Effect.Console (log)

import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int as PI
import Prim.Ordering as PO
import Type.Proxy (Proxy(..))

-- | Valid bounds are i where 1 <= i <= 5
newtype TestAddCompare = TestAddCompare Int

type TestAddCompareLower :: Int
type TestAddCompareLower = 1

type TestAddCompareUpper :: Int
type TestAddCompareUpper = 5

mkTestAddCompare
  :: forall i lower upper
   . Reflectable i Int
  => PI.Add TestAddCompareLower (-1) lower
  => PI.Compare i lower PO.GT
  => PI.Add TestAddCompareUpper 1 upper
  => PI.Compare i upper PO.LT
  => Proxy i
  -> TestAddCompare
mkTestAddCompare p = TestAddCompare (reflectType p)

data AddCompare = AddCompare TestAddCompare Int

mkAddCompare
  :: forall i lower upper
   . Reflectable i Int
  => PI.Add TestAddCompareLower (-1) lower
  => PI.Compare i lower PO.GT
  => PI.Add TestAddCompareUpper 1 upper
  => PI.Compare i upper PO.LT
  => Proxy i
  -> Int
  -> AddCompare
mkAddCompare p i = AddCompare (mkTestAddCompare p) i

addCompare :: AddCompare
addCompare = mkAddCompare (Proxy :: Proxy 3) 1


-- | Valid values are i where (i * 4) < 100
newtype TestMulCompare = TestMulCompare Int

type TestMulCompareUpper :: Int
type TestMulCompareUpper = 100

type TestMulCompareScale :: Int
type TestMulCompareScale = 4

mkTestMulCompare
  :: forall i val upper
   . Reflectable i Int
  => PI.Mul i TestMulCompareScale val
  => PI.Compare val TestMulCompareUpper PO.LT
  => Proxy i
  -> TestMulCompare
mkTestMulCompare p = TestMulCompare (reflectType p)

data MulCompare = MulCompare TestMulCompare Int

mkMulCompare
  :: forall i val upper
   . Reflectable i Int
  => PI.Mul i TestMulCompareScale val
  => PI.Compare val TestMulCompareUpper PO.LT
  => Proxy i
  -> Int
  -> MulCompare
mkMulCompare p i = MulCompare (mkTestMulCompare p) i

mulCompare :: MulCompare
mulCompare = mkMulCompare (Proxy :: Proxy 3) 1


-- | Valid values are i where 2 <= i <= 100
newtype TestAddMulCompare = TestAddMulCompare Int


mkTestAddMulCompare
  :: forall i one two
   . Reflectable i Int
  => PI.Add 0 1 one
  => PI.Mul one 2 two
  => PI.Compare i two PO.GT
  => Proxy i
  -> TestAddMulCompare
mkTestAddMulCompare p = TestAddMulCompare (reflectType p)

data AddMulCompare = AddMulCompare TestAddMulCompare Int

mkAddMulCompare
  :: forall i one two
   . Reflectable i Int
  => PI.Add 0 1 one
  => PI.Mul one 2 two
  => PI.Compare i two PO.GT
  => Proxy i
  -> Int
  -> AddMulCompare
mkAddMulCompare p i = AddMulCompare (mkTestAddMulCompare p) i

addMulCompare :: AddMulCompare
addMulCompare = mkAddMulCompare (Proxy :: Proxy 8) 1


main = log "Done"
