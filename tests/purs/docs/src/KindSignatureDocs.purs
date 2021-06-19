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
-- | CKindAndType - type docs
class CKindAndType :: forall k. (k -> Type) -> k -> Constraint
class CKindAndType a k where
  fooKindAndType :: a k -> String

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
class CKindOnly :: forall k. (k -> Type) -> k -> Constraint
class CKindOnly a k where
  fooKindOnly :: a k -> String

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

-- | CTypeOnly - type docs
class CTypeOnly :: forall k. (k -> Type) -> k -> Constraint
class CTypeOnly a k where
  fooTypeOnly :: a k -> String
