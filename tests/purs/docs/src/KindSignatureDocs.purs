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

foreign import data FFI_Hidden :: Type -> Type -> Type
foreign import data FFI_Shown :: (Type -> Type) -> Type

----------

data DataRedundantParenthesis :: (Type) -> (Type)
data DataRedundantParenthesis a = DataRedundantParenthesis

class ClassRedundantParenthesis :: (Type) -> (Constraint)
class ClassRedundantParenthesis a

data DataHeadParens :: (Type) -> Type -> Type
data DataHeadParens a b = DataHeadParens

data DataTailParens :: Type -> (Type -> Type)
data DataTailParens a b = DataTailParens

data DataWholeParens :: (Type -> Type -> Type)
data DataWholeParens a b = DataWholeParens

data DataSelfParens :: (Type)
data DataSelfParens = DataSelfParens

class ClassSelfParens :: (Constraint)
class ClassSelfParens

data DataKindAnnotation (a :: Type) = DataKindAnnotation a

data DataKindAnnotationWithParens (a :: (Type)) = DataKindAnnotationWithParens a

data FunctionParens1 :: (->) Type Type
data FunctionParens1 a = FunctionParens1 a

data FunctionParens2 :: ((->) Type) Type
data FunctionParens2 a = FunctionParens2 a

data FunctionParens3 :: (((->) Type)) Type
data FunctionParens3 a = FunctionParens3 a
----------

data DShown a b f = DShown (f Int) a b

type TShown f b c = DShown b c f

newtype NShown a f c = NShown (DShown a c f)

class CShown f a b where
  fooShown :: f Int -> a -> b -> String
