module RoleAnnotationDocs where

data D_RNP a b c = D_RNP
type role D_RNP representational nominal phantom

data D_NPR a b c = D_NPR
type role D_NPR nominal phantom representational

data D_PRN a b c = D_PRN
type role D_PRN phantom representational nominal

foreign import data FFI_NNN :: Type -> Type -> Type -> Type

foreign import data FFI_RNP :: Type -> Type -> Type -> Type
type role FFI_RNP representational nominal phantom

foreign import data FFI_HeadParens :: (Type) -> Type -> Type -> Type
type role FFI_HeadParens representational nominal phantom

-- This produces a compiler error as of v0.14.2
-- foreign import data FFI_TailParens :: Type -> (Type -> Type -> Type)
-- type role FFI_TailParens representational nominal phantom

-- This produces a compiler error as of v0.14.2
-- foreign import data FFI_WholeParens :: (Type -> Type -> Type -> Type)
-- type role FFI_WholeParens representational nominal phantom
