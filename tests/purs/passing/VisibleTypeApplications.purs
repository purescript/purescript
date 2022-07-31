module Main where

import Prelude

import Effect.Console (log)

data Tuple a b = Tuple a b

infixr 5 type Tuple as /\

tuple :: forall @a @b. a -> b -> Tuple a b
tuple = Tuple

tuple' :: Tuple Int Int
tuple' = tuple @Int @Int 21 42

data Id a = Id a

id :: forall @a. a -> Id a
id = Id

id' :: Id (forall a. a -> a)
id' = id @(forall a. a -> a) (\x -> x)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

syn :: Proxy (Tuple Int Int)
syn = Proxy @(Int /\ Int)

class TypeClass1 a

instance TypeClass1 Int

tc1 :: forall @a. TypeClass1 a => Proxy a
tc1 = Proxy

tc1' :: Proxy Int
tc1' = tc1 @Int

class TypeClass2 a b | a -> b

instance TypeClass2 Int String

tc2 :: forall @a b. TypeClass2 a b => Proxy ( a :: a, b :: b )
tc2 = Proxy

tc2' :: Proxy ( a :: Int, b :: String )
tc2' = tc2 @Int

class TypeClass3 @f where
  tc3 :: Proxy ( f :: f )

instance TypeClass3 Id where
  tc3 = Proxy

tc3' :: Proxy ( f :: Id )
tc3' = tc3 @Id

kinded :: forall @k (@a :: k) j (@b :: j). Proxy (a :: Proxy a, k :: Proxy k, b :: Proxy b, j :: Proxy j)
kinded = Proxy

kinded' :: Proxy ( a :: Proxy "Int", b :: Proxy "Int", j :: Proxy Symbol, k :: Proxy Symbol )
kinded' = kinded @Symbol @"Int" @"Int"

changeId :: forall a. a -> a
changeId x = x

changeId' :: Int -> Int
changeId' = (changeId :: forall @a. a -> a) @Int

data Either a b = Left a | Right b

left :: Either Int String
left = Left @Int @String 0

right :: Either Int String
right = Right @Int @String "0"

leftSkip :: Either Int String
leftSkip = Left @_ @String 0

rightSkip :: Either Int String
rightSkip = Right @Int @_ "0"

hrk :: forall (@f :: forall k. k -> Type). Proxy f
hrk = Proxy

hrk' :: Proxy Proxy
hrk' = hrk @Proxy

kindCheck :: forall @k (@t :: k). Proxy t
kindCheck = Proxy

kindCheck' :: Proxy "Type"
kindCheck' = kindCheck @Symbol @"Type"

newtype Id' a = Id' a

_IdInt :: Int -> Id' Int
_IdInt = Id' @Int

whereTest :: forall a b. a -> b -> String
whereTest a b = foo' @a a <> bar' @b b
  where
  foo' :: forall @a. a -> String
  foo' _ = "foo"

  bar' :: forall @b. b -> String
  bar' _ = "bar"

type Foo = forall @a. { foo :: a }
type Foo2 = { foo :: forall @a. a -> String }

testRank2 :: (forall @b. b -> String) -> String
testRank2 f = f 1 <> f true

testRank2' :: String
testRank2' = testRank2 (\_ -> "rank2")

-- whether `BaseNoVta` or `BaseVta`
-- have visible type application type variable
-- for `a` or not`, a super class still works
class BaseNoVta a where
  baseNoVta :: a -> String

class BaseNoVta a <= Super1 @a where
  super1 :: a -> a -> String

class BaseVta @a where
  baseVta :: a -> String

class BaseVta a <= Super2 @a where
  super2 :: a -> a -> String

instance BaseNoVta String where
  baseNoVta x = x

instance Super1 String where
  super1 = (<>)

instance BaseVta String where
  baseVta x = x

instance Super2 String where
  super2 = (<>)

testClassRelationship :: String
testClassRelationship = super1 "foo" "bar" <> super2 "foo" "bar"

-- an abstracted type variable is usable
class Usable @a where
  usable :: String

main = log "Done"
