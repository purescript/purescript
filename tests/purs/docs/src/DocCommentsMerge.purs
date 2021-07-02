module DocCommentsMerge where

-- | decl
data DeclOnly = DeclOnly

-- | kind
data KindAndDecl :: Type -> Type -> Type
-- | decl
data KindAndDecl a b = KindAndDecl

-- | decl
data DeclAndRole a b = DeclAndRole a b
-- | role
type role DeclAndRole representational representational

-- | kind
data KindDeclAndRole :: Type -> Type
-- | decl
data KindDeclAndRole a = KindDeclAndRole
-- | role
type role KindDeclAndRole representational


-- | decl
newtype NewtypeOnly = NewtypeOnly Int

-- | kind
newtype KindAndNewtype :: Type -> Type -> Type
-- | decl
newtype KindAndNewtype a b = KindAndNewtype Int

-- | decl
newtype NewtypeAndRole a b = NewtypeAndRole Int
-- | role
type role NewtypeAndRole representational representational

-- | kind
newtype KindNewtypeAndRole :: Type -> Type -> Type
-- | decl
newtype KindNewtypeAndRole a b = KindNewtypeAndRole Int
-- | role
type role KindNewtypeAndRole representational representational


-- | decl
type TypeOnly = Int

-- | kind
type KindAndType :: Type -> Type -> Type
-- | decl
type KindAndType a b = Int

-- type can't have role annotations


-- | decl
class ClassOnly

-- | kind
class KindAndClass :: Type -> Constraint
-- | decl
class KindAndClass a where
  fooKindAndClass :: a -> String

-- class can't have role declarations
