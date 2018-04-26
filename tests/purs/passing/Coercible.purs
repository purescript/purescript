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
newtype Id2 a = Id2 a

id1ToId2 :: forall a. Id1 a -> Id2 a
id1ToId2 = coerce

id12ToId21 :: forall b. Id1 (Id2 b) -> Id2 (Id1 b)
id12ToId21 = coerce

newtype Phantom1 a b = Phantom1 a

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

data Phantom2 a = Phantom

data Maybe a = Nothing | Just a

data G a b = G (a (Phantom2 b))

gToG :: G Maybe Int -> G Maybe String
gToG = coerce

foreign import data Foreign1 :: Type -> Type -> Type

foreign1ToForeign1 :: Foreign1 NTString1 (Phantom2 Int) -> Foreign1 String (Phantom2 Boolean)
foreign1ToForeign1 = coerce

foreign import data Foreign2 :: Type -> Type -> Type

role Foreign2 phantom representational

foreign2ToForeign2 :: Foreign2 NTString2 (Phantom2 Int) -> Foreign2 Int (Phantom2 Boolean)
foreign2ToForeign2 = coerce

main = log (coerce (NTString1 "Done") :: String)
