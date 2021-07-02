module RoleAnnotationDocs where

data D_RNP a b c = D_RNP
type role D_RNP representational nominal phantom

data D_NPR a b c = D_NPR
type role D_NPR nominal phantom representational

data D_PRN a b c = D_PRN
type role D_PRN phantom representational nominal

type T_RNP :: Type -> Type -> Type -> Type
type T_RNP a b c = Int
type role T_RNP representational nominal phantom

type T_NPR :: Type -> Type -> Type -> Type
type T_NPR a b c = Int
type role T_NPR nominal phantom representational

type T_PRN :: Type -> Type -> Type -> Type
type T_PRN a b c = Int
type role T_PRN phantom representational nominal
