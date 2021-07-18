module DocCommentsMerge where

-- | decl
data DataOnly = DataOnly

-- | kind
data KindOnlyData :: Type
data KindOnlyData = KindOnlyData

-- | kind
data KindAndData :: Type
-- | decl
data KindAndData = KindAndData

data DataRoleOnly a b = DataRoleOnly a b
-- | role
type role DataRoleOnly representational representational

-- | decl
data DataAndRole a b = DataAndRole a b
-- | role
type role DataAndRole representational representational

-- | kind
data KindOnlyDataRoleOnly :: Type -> Type
data KindOnlyDataRoleOnly a = KindOnlyDataRoleOnly
-- | role
type role KindOnlyDataRoleOnly representational

-- | kind
data KindDataAndRole :: Type -> Type
-- | decl
data KindDataAndRole a = KindDataAndRole
-- | role
type role KindDataAndRole representational

---

-- | decl
foreign import data FFIOnly :: Type

foreign import data FFIRoleOnly :: Type -> Type
-- | role
type role FFIRoleOnly representational

-- | decl
foreign import data FFIAndRole :: Type -> Type
-- | role
type role FFIAndRole representational

---

-- | decl
newtype NewtypeOnly = NewtypeOnly Int

-- | kind
newtype KindOnlyNewtype :: Type
newtype KindOnlyNewtype = KindOnlyNewtype Int

-- | kind
newtype KindAndNewtype :: Type -> Type -> Type
-- | decl
newtype KindAndNewtype a b = KindAndNewtype Int

newtype NewtypeRoleOnly a b = NewtypeRoleOnly Int
-- | role
type role NewtypeRoleOnly representational representational

-- | decl
newtype NewtypeAndRole a b = NewtypeAndRole Int
-- | role
type role NewtypeAndRole representational representational

-- | kind
newtype KindOnlyNewtypeRoleOnly :: Type -> Type -> Type
newtype KindOnlyNewtypeRoleOnly a b = KindOnlyNewtypeRoleOnly Int
-- | role
type role KindOnlyNewtypeRoleOnly representational representational

-- | kind
newtype KindNewtypeAndRole :: Type -> Type -> Type
-- | decl
newtype KindNewtypeAndRole a b = KindNewtypeAndRole Int
-- | role
type role KindNewtypeAndRole representational representational

---

-- | decl
type TypeOnly = Int

-- | kind
type KindOnlyType :: Type -> Type -> Type
type KindOnlyType a b = Int

-- | kind
type KindAndType :: Type -> Type -> Type
-- | decl
type KindAndType a b = Int

-- type can't have role annotations

---

-- | decl
class ClassOnly

-- | kind
class KindOnlyClass :: Constraint
class KindOnlyClass

-- | kind
class KindAndClass :: Type -> Constraint
-- | decl
class KindAndClass a where
  fooKindAndClass :: a -> String

-- class can't have role declarations
