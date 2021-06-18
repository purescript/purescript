module KindSignatureDocs where

-- | DKindAndType - kind docs
data DKindAndType :: forall k. k -> Type
-- | DKindAndType - type docs
data DKindAndType a = DKindAndType

-- | TKindAndType - kind docs
type TKindAndType :: forall k. k -> Type
-- | TKindAndType - type docs
type TKindAndType a = Int

-- | NKindAndType - kind docs
newtype NKindAndType :: forall k. k -> Type
-- | NKindAndType - type docs
newtype NKindAndType a = NKindAndType Int

-- | CKindAndType - kind docs
class CKindAndType :: Type -> Constraint
-- | CKindAndType - type docs
class CKindAndType a where
  fooKindAndType :: a -> String

----------

-- | DKindOnly - kind docs
data DKindOnly :: forall k. k -> Type
data DKindOnly a = DKindOnly

-- | TKindOnly - kind docs
type TKindOnly :: forall k. k -> Type
type TKindOnly a = Int

-- | NKindOnly - kind docs
newtype NKindOnly :: forall k. k -> Type
newtype NKindOnly a = NKindOnly Int

-- | CKindOnly - kind docs
class CKindOnly :: Type -> Constraint
class CKindOnly a where
  fooKindOnly :: a -> String

----------

data DTypeOnly :: forall k. k -> Type
-- | DTypeOnly - type docs
data DTypeOnly a = DTypeOnly

type TTypeOnly :: forall k. k -> Type
-- | TTypeOnly - type docs
type TTypeOnly a = Int

newtype NTypeOnly :: forall k. k -> Type
-- | NTypeOnly - type docs
newtype NTypeOnly a = NTypeOnly Int

class CTypeOnly :: Type -> Constraint
-- | CTypeOnly - type docs
class CTypeOnly a where
  fooTypeOnly :: a -> String
