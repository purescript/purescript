module Main where

import Coercible.Lib

import Effect.Console (log)
import Safe.Coerce (coerce)

type SynString = String

newtype NTString1 = NTString1 SynString

nt1ToString :: NTString1 -> String
nt1ToString = coerce

newtype NTString2 = NTString2 String

nt2ToNT1 :: NTString2 -> NTString1
nt2ToNT1 = coerce

newtype Id1 a = Id1 a
newtype Id2 b = Id2 b

id1ToId2 :: forall a. Id1 a -> Id2 a
id1ToId2 = coerce

id12ToId21 :: forall b. Id1 (Id2 b) -> Id2 (Id1 b)
id12ToId21 = coerce

newtype Ap f a = Ap (f a)

apId1ToApId2 :: forall a. Ap Id1 a -> Ap Id2 a
apId1ToApId2 = coerce

newtype Phantom1 a b = Phantom1 a

phantom1TypeToPhantom1Symbol :: forall x (y :: Type) (z :: Symbol). Phantom1 x y -> Phantom1 x z
phantom1TypeToPhantom1Symbol = coerce

phantom1ToId12 :: forall x y. Phantom1 x y -> Id1 (Id2 x)
phantom1ToId12 = coerce

nested :: forall x y z. Phantom1 (Id1 (Phantom1 x y)) y -> Id2 (Phantom1 x (Phantom1 z z))
nested = coerce

id1IntToInt :: Id1 Int -> Int
id1IntToInt = coerce

id2IntToId1Int :: Id2 Int -> Id1 Int
id2IntToId1Int = coerce

newtype NTInt1 = NTInt1 Int

id2NTToId1Nt :: Id2 NTInt1 -> Id1 NTInt1
id2NTToId1Nt = coerce

id2NTToId1Int :: Id2 NTInt1 -> Id1 Int
id2NTToId1Int = coerce

newtype NTFn1 a b   = NTFn1 (a -> Int -> b)
newtype NTFn2 x a b = NTFn2 (a -> x -> b)

ntFn1ToNTFn2 :: forall a b. NTFn1 a b -> NTFn2 Int a b
ntFn1ToNTFn2 = coerce

libExposedCtorToId2 :: forall z. NTLib z -> Id2 z
libExposedCtorToId2 = coerce

newtype Roles1 a b c = Roles1 (Phantom1 b c)

roles1ToSecond :: forall r s t. Roles1 r s t -> s
roles1ToSecond = coerce

data D a b = D a

underD :: D NTString1 Boolean -> D NTString2 Int
underD = coerce

newtype NTD a b c d = NTD (D b d)

dToNTD :: forall i j k l. D j l -> NTD i (Id1 j) k (Phantom1 l k)
dToNTD = coerce

ntdToNTD :: forall i j k l. NTD i j k l -> NTD (Id1 k) (Phantom1 j k) Int Boolean
ntdToNTD = coerce

newtype RankN1 a b = RankN1 (forall r. r -> a)

rankN1ToRankN1 :: RankN1 NTString1 Int -> RankN1 String Boolean
rankN1ToRankN1 = coerce

data RankN2 a = RankN2 (forall a. a -> a)

rankN2ToRankN2 :: forall x y. RankN2 x -> RankN2 y
rankN2ToRankN2 = coerce

data RankN3 c = RankN3 (forall c. (forall c. c -> c) -> c)

rankN3ToRankN3 :: forall x y. RankN3 x -> RankN3 y
rankN3ToRankN3 = coerce

data RankN4 z = RankN4 (forall c. (forall z. c -> z) -> c)

rankN4ToRankN4 :: forall x y. RankN4 x -> RankN4 y
rankN4ToRankN4 = coerce

data Phantom2 a = Phantom

data Rec1 a = Rec1 { f :: a }

rec1ToRec1 :: Rec1 Int -> Rec1 (Id1 Int)
rec1ToRec1 = coerce

data Rec2 a b = Rec2 { f :: a, g :: Int, h :: b }

rec2ToRec2 :: Rec2 Int (Phantom2 String) -> Rec2 (Id1 Int) (Phantom2 Int)
rec2ToRec2 = coerce

data Rec3 a = Rec3 {}

rec3ToRec3 :: forall m n. Rec3 m -> Rec3 n
rec3ToRec3 = coerce

newtype Rec4 f = Rec4 (f {})

unwrapRec4 :: forall f. Rec4 f -> f {}
unwrapRec4 = coerce

newtype Rec5 a f = Rec5 (f {})

apRec4ToApRec5 :: forall a. Ap Rec4 Id1 -> Ap (Rec5 a) Id1
apRec4ToApRec5 = coerce

data Arr1 a b = Arr1 (Array a) (Array b)

arr1ToArr1 :: Arr1 Int String -> Arr1 (Id1 Int) (Id2 String)
arr1ToArr1 = coerce

arr1ToArr1Phantom :: forall a. Arr1 (Phantom2 Int) String -> Arr1 (Phantom2 a) (Id2 String)
arr1ToArr1Phantom = coerce

foreign import data Foreign1 :: Type -> Type -> Type

type role Foreign1 representational representational

foreign1ToForeign1 :: Foreign1 NTString1 (Phantom2 Int) -> Foreign1 String (Phantom2 Boolean)
foreign1ToForeign1 = coerce

foreign import data Foreign2 :: Type -> Type -> Type

type role Foreign2 phantom representational

foreign2ToForeign2 :: Foreign2 NTString2 (Phantom2 Int) -> Foreign2 Int (Phantom2 Boolean)
foreign2ToForeign2 = coerce

data MyMap k v = MyMap k v

type role MyMap nominal representational

mapToMap :: MyMap String String -> MyMap String NTString1
mapToMap = coerce

-- "role" should only be a reserved word after "type"
testRoleNotReserved :: String -> String
testRoleNotReserved role = role

-- "nominal", "representational" and "phantom" should only be reserved when in
-- role signatures
testRolesNotReserved :: String -> String -> String -> String
testRolesNotReserved nominal representational phantom = ""

data RoleNotReserved role = RoleNotReserved role

-- Contextual keywords should be allowed unquoted in rows.
type ContextualKeywords =
  ( nominal :: String
  , phantom :: String
  , representational :: String
  , role :: String
  )

data MutuallyRecursivePhantom1 a
  = MutuallyRecursivePhantom1 (MutuallyRecursivePhantom2 a)

data MutuallyRecursivePhantom2 a
  = MutuallyRecursivePhantom2 (MutuallyRecursivePhantom1 a)

mutuallyRecursivePhantom :: forall a b. MutuallyRecursivePhantom1 a -> MutuallyRecursivePhantom1 b
mutuallyRecursivePhantom = coerce

data MutuallyRecursiveRepresentational1 a
  = MutuallyRecursiveRepresentational1 a (MutuallyRecursiveRepresentational2 a)

data MutuallyRecursiveRepresentational2 a
  = MutuallyRecursiveRepresentational2 (MutuallyRecursiveRepresentational1 a)

mutuallyRecursiveRepresentational :: forall a. MutuallyRecursiveRepresentational1 a -> MutuallyRecursiveRepresentational1 (Id1 a)
mutuallyRecursiveRepresentational = coerce

main = log (coerce (NTString1 "Done") :: String)
