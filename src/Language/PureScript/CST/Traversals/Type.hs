module Language.PureScript.CST.Traversals.Type where

import Prelude

import Language.PureScript.CST.Types
import Language.PureScript.CST.Traversals

everythingOnTypes :: (r -> r -> r) -> (Type a -> r) -> Type a -> r
everythingOnTypes op k = goTy
  where
  goTy ty = case ty of
    TypeVar _ _ -> k ty
    TypeConstructor _ _ -> k ty
    TypeWildcard _ _ -> k ty
    TypeHole _ _ -> k ty
    TypeString _ _ _ -> k ty
    TypeRow _ (Wrapped _ row _) -> goRow ty row
    TypeRecord _ (Wrapped _ row _) -> goRow ty row
    TypeForall _ _ _ _ ty2 -> k ty `op` goTy ty2
    TypeKinded _ ty2 _ _ -> k ty `op` goTy ty2
    TypeApp _ ty2 ty3 -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeOp _ ty2 _ ty3 -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeOpName _ _ -> k ty
    TypeArr _ ty2 _ ty3 -> k ty `op` (goTy ty2 `op` goTy ty3)
    TypeArrName _ _ -> k ty
    TypeConstrained _ (constraintTys -> ty2) _ ty3
      | null ty2 -> k ty `op` goTy ty3
      | otherwise -> k ty `op` (foldr1 op (k <$> ty2) `op` goTy ty3)
    TypeParens _ (Wrapped _ ty2 _) -> k ty `op` goTy ty2

  goRow ty = \case
    Row Nothing Nothing -> k ty
    Row Nothing (Just (_, ty2)) -> k ty `op` goTy ty2
    Row (Just lbls) Nothing -> k ty `op` everythingOnSeparated op (goTy . lblValue) lbls
    Row (Just lbls) (Just (_, ty2)) -> k ty `op` (everythingOnSeparated op (goTy . lblValue) lbls `op` goTy ty2)

  constraintTys = \case
    Constraint _ _ tys -> tys
    ConstraintParens _ (Wrapped _ c _) -> constraintTys c
