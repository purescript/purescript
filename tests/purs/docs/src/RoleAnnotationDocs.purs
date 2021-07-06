module RoleAnnotationDocs where

data D_RNP a b c = D_RNP
type role D_RNP representational nominal phantom

data D_NPR a b c = D_NPR
type role D_NPR nominal phantom representational

data D_PRN a b c = D_PRN
type role D_PRN phantom representational nominal

foreign import data E_NNN :: Type -> Type -> Type -> Type

foreign import data E_RNP :: Type -> Type -> Type -> Type
type role E_RNP representational nominal phantom
