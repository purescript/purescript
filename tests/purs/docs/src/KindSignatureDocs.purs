module KindSignatureDocs where

data DKindAndType :: forall k. k -> Type
data DKindAndType a = DKindAndType

type TKindAndType :: forall k. k -> Type
type TKindAndType a = Int

newtype NKindAndType :: forall k. k -> Type
newtype NKindAndType a = NKindAndType Int

class CKindAndType :: forall k. (k -> Type) -> k -> Constraint
class CKindAndType a k where
  fooKindAndType :: a k -> String

----------

data DKindOnly :: forall k. k -> Type
data DKindOnly a = DKindOnly

type TKindOnly :: forall k. k -> Type
type TKindOnly a = Int

newtype NKindOnly :: forall k. k -> Type
newtype NKindOnly a = NKindOnly Int

class CKindOnly :: forall k. (k -> Type) -> k -> Constraint
class CKindOnly a k where
  fooKindOnly :: a k -> String

----------

data DTypeOnly :: forall k. k -> Type
data DTypeOnly a = DTypeOnly

type TTypeOnly :: forall k. k -> Type
type TTypeOnly a = Int

newtype NTypeOnly :: forall k. k -> Type
newtype NTypeOnly a = NTypeOnly Int

class CTypeOnly :: forall k. (k -> Type) -> k -> Constraint
class CTypeOnly a k where
  fooTypeOnly :: a k -> String

----------

data DImplicit a = DImplicit

type TImplicit a = Int

newtype NImplicit a = NImplicit Int

class CImplicit a k where
  fooImplicit :: a k -> String

----------

data DHidden a b c = DHidden a b c

data DNothing

type THidden a b c = DHidden b c a

newtype NHidden a b c = NHidden (DHidden a c b)

class CHidden a b c where
  fooHidden :: a -> b -> c -> String

class CNothing

----------

data DShown a b f = DShown (f Int) a b

type TShown f b c = DShown b c f

newtype NShown a f c = NShown (DShown a c f)

class CShown f a b where
  fooShown :: f Int -> a -> b -> String
