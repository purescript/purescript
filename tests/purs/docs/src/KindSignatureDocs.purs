module KindSignatureDocs where

-- | dkatk
data DKindAndType :: forall k. k -> Type
-- | dkatt
data DKindAndType a = DKindAndType

-- | tkatk
type TKindAndType :: forall k. k -> Type
-- | tkatt
type TKindAndType a = Int

-- | nkatk
newtype NKindAndType :: forall k. k -> Type
-- | nkatt
newtype NKindAndType a = NKindAndType Int

-- | ckatk
class CKindAndType :: forall k. (k -> Type) -> k -> Constraint
-- | ckatt
class CKindAndType a k where
  fooKindAndType :: a k -> String

----------

-- | dkok
data DKindOnly :: forall k. k -> Type
data DKindOnly a = DKindOnly

-- | tkok
type TKindOnly :: forall k. k -> Type
type TKindOnly a = Int

-- | nkok
newtype NKindOnly :: forall k. k -> Type
newtype NKindOnly a = NKindOnly Int

-- | ckok
class CKindOnly :: forall k. (k -> Type) -> k -> Constraint
class CKindOnly a k where
  fooKindOnly :: a k -> String

----------

data DTypeOnly :: forall k. k -> Type
-- | dtot
data DTypeOnly a = DTypeOnly

type TTypeOnly :: forall k. k -> Type
-- | ttot
type TTypeOnly a = Int

newtype NTypeOnly :: forall k. k -> Type
-- | ntot
newtype NTypeOnly a = NTypeOnly Int

class CTypeOnly :: forall k. (k -> Type) -> k -> Constraint
-- | ctot
class CTypeOnly a k where
  fooTypeOnly :: a k -> String

----------

-- | dit
data DImplicit a = DImplicit

-- | tit
type TImplicit a = Int

-- | nit
newtype NImplicit a = NImplicit Int

-- | cit
class CImplicit a k where
  fooImplicit :: a k -> String

----------

-- | dit
data DHidden a b c = DHidden a b c

data DNothing

-- | tit
type THidden a b c = DHidden b c a

-- | nit
newtype NHidden a b c = NHidden (DHidden a c b)

-- | cit
class CHidden a b c where
  fooHidden :: a -> b -> c -> String

class CNothing

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

-- | dit
data DShown a b f = DShown (f Int) a b

-- | tit
type TShown f b c = DShown b c f

-- | nit
newtype NShown a f c = NShown (DShown a c f)

-- | cit
class CShown f a b where
  fooShown :: f Int -> a -> b -> String
