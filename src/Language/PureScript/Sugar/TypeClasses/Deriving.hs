-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Sugar.TypeClasses.Deriving
-- Copyright   :  (c) Gershom Bazerman 2015
-- License     :  MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the generic deriving elaboration that takes place during desugaring.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Sugar.TypeClasses.Deriving (
    deriveInstances
) where

import Data.List
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad (replicateM)
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

-- | Elaborates deriving instance declarations by code generation.
deriveInstances ::  (Functor m, Applicative m, MonadError MultipleErrors m, MonadSupply m) => Module -> m Module
deriveInstances (Module ss coms mn ds exts) = Module ss coms mn <$> mapM (deriveInstance mn ds) ds <*> pure exts

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
deriveInstance :: (Functor m, MonadError MultipleErrors m, MonadSupply m) => ModuleName -> [Declaration] -> Declaration -> m Declaration
deriveInstance mn ds (TypeInstanceDeclaration nm deps className tys@[ty] DerivedInstance)
  | className == Qualified (Just dataGeneric) (ProperName C.generic)
  , Just (Qualified mn' tyCon) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveGeneric mn ds tyCon
deriveInstance _ _ (TypeInstanceDeclaration _ _ className tys DerivedInstance)
  = throwError . errorMessage $ CannotDerive className tys
deriveInstance mn ds (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> deriveInstance mn ds d
deriveInstance _  _  e = return e

unwrapTypeConstructor :: Type -> Maybe (Qualified ProperName)
unwrapTypeConstructor (TypeConstructor tyCon) = Just tyCon
unwrapTypeConstructor (TypeApp ty (TypeVar _)) = unwrapTypeConstructor ty
unwrapTypeConstructor _ = Nothing

dataGeneric :: ModuleName
dataGeneric = ModuleName [ ProperName "Data", ProperName "Generic" ]

dataMaybe :: ModuleName
dataMaybe = ModuleName [ ProperName "Data", ProperName "Maybe" ]

deriveGeneric :: (Functor m, MonadError MultipleErrors m, MonadSupply m) => ModuleName -> [Declaration] -> ProperName -> m [Declaration]
deriveGeneric mn ds tyConNm = do
  tyCon <- findTypeDecl tyConNm ds
  toSpine <- mkSpineFunction mn tyCon
  fromSpine <- mkFromSpineFunction mn tyCon
  let toSignature = mkSignatureFunction mn tyCon
  return [ ValueDeclaration (Ident C.toSpine) Public [] (Right toSpine)
         , ValueDeclaration (Ident C.fromSpine) Public [] (Right fromSpine)
         , ValueDeclaration (Ident C.toSignature) Public [] (Right toSignature)
         ]

findTypeDecl :: (Functor m, MonadError MultipleErrors m) => ProperName -> [Declaration] -> m Declaration
findTypeDecl tyConNm = maybe (throwError . errorMessage $ CannotFindDerivingType tyConNm) return . find isTypeDecl
  where
  isTypeDecl :: Declaration -> Bool
  isTypeDecl (DataDeclaration _ nm _ _) | nm == tyConNm = True
  isTypeDecl (PositionedDeclaration _ _ d) = isTypeDecl d
  isTypeDecl _ = False

mkSpineFunction :: forall m. (Functor m, MonadSupply m) => ModuleName -> Declaration -> m Expr
mkSpineFunction mn (DataDeclaration _ _ _ args) = lamCase "$x" <$> mapM mkCtorClause args
  where
  prodConstructor :: Expr -> Expr
  prodConstructor = App (Constructor (Qualified (Just dataGeneric) (ProperName "SProd")))

  recordConstructor :: Expr -> Expr
  recordConstructor = App (Constructor (Qualified (Just dataGeneric) (ProperName "SRecord")))

  mkCtorClause :: (ProperName, [Type]) -> m CaseAlternative
  mkCtorClause (ctorName, tys) = do
    idents <- replicateM (length tys) (fmap Ident freshName)
    return $ CaseAlternative [ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)] (Right (caseResult idents))
    where
    caseResult idents =
      App (prodConstructor (StringLiteral . runProperName $ ctorName))
        . ArrayLiteral
        $ zipWith toSpineFun (map (Var . Qualified Nothing) idents) tys

  toSpineFun :: Expr -> Type -> Expr
  toSpineFun i r | Just rec <- objectType r =
      lamNull . recordConstructor . ArrayLiteral .
          map (\(str,typ) -> ObjectLiteral [("recLabel", StringLiteral str), ("recValue", toSpineFun (Accessor str i) typ)])
          $ decomposeRec rec
  toSpineFun i _ = lamNull $ App (mkGenVar C.toSpine) i
mkSpineFunction mn (PositionedDeclaration _ _ d) = mkSpineFunction mn d
mkSpineFunction _ _ = internalError "mkSpineFunction: expected DataDeclaration"

mkSignatureFunction :: ModuleName -> Declaration -> Expr
mkSignatureFunction _ (DataDeclaration _ _ _ args) = lamNull . mkSigProd $ map mkProdClause args
  where
  mkSigProd :: [Expr] -> Expr
  mkSigProd = App (Constructor (Qualified (Just dataGeneric) (ProperName "SigProd"))) . ArrayLiteral

  mkSigRec :: [Expr] -> Expr
  mkSigRec = App (Constructor (Qualified (Just dataGeneric) (ProperName "SigRecord"))) . ArrayLiteral

  proxy :: Type -> Type
  proxy = TypeApp (TypeConstructor (Qualified (Just dataGeneric) (ProperName "Proxy")))

  mkProdClause :: (ProperName, [Type]) -> Expr
  mkProdClause (ctorName, tys) = ObjectLiteral [ ("sigConstructor", StringLiteral (runProperName ctorName))
                                               , ("sigValues", ArrayLiteral . map mkProductSignature $ tys)
                                               ]

  mkProductSignature :: Type -> Expr
  mkProductSignature r | Just rec <- objectType r =
      lamNull . mkSigRec $ [ ObjectLiteral [ ("recLabel", StringLiteral str)
                                           , ("recValue", mkProductSignature typ)
                                           ]
                           | (str, typ) <- decomposeRec rec
                           ]
  mkProductSignature typ = lamNull $ App (mkGenVar C.toSignature)
                           (TypedValue False (mkGenVar "anyProxy") (proxy typ))
mkSignatureFunction mn (PositionedDeclaration _ _ d) = mkSignatureFunction mn d
mkSignatureFunction _ _ = internalError "mkSignatureFunction: expected DataDeclaration"

mkFromSpineFunction :: forall m. (Functor m, MonadSupply m) => ModuleName -> Declaration -> m Expr
mkFromSpineFunction mn (DataDeclaration _ _ _ args) = lamCase "$x" <$> (addCatch <$> mapM mkAlternative args)
  where
  mkJust :: Expr -> Expr
  mkJust = App (Constructor (Qualified (Just dataMaybe) (ProperName "Just")))

  mkNothing :: Expr
  mkNothing = Constructor (Qualified (Just dataMaybe) (ProperName "Nothing"))

  prodBinder :: [Binder] -> Binder
  prodBinder = ConstructorBinder (Qualified (Just dataGeneric) (ProperName "SProd"))

  recordBinder :: [Binder] -> Binder
  recordBinder = ConstructorBinder (Qualified (Just dataGeneric) (ProperName "SRecord"))

  mkAlternative :: (ProperName, [Type]) -> m CaseAlternative
  mkAlternative (ctorName, tys) = do
    idents <- replicateM (length tys) (fmap Ident freshName)
    return $ CaseAlternative [ prodBinder [ StringBinder (runProperName ctorName), ArrayBinder (map VarBinder idents)]]
               . Right
               $ liftApplicative (mkJust $ Constructor (Qualified (Just mn) ctorName))
                                 (zipWith fromSpineFun (map (Var . (Qualified Nothing)) idents) tys)

  addCatch :: [CaseAlternative] -> [CaseAlternative]
  addCatch = (++ [catchAll])
    where
    catchAll = CaseAlternative [NullBinder] (Right mkNothing)

  fromSpineFun e r
    | Just rec <- objectType r
    = App (lamCase "r" [ mkRecCase (decomposeRec rec)
                       , CaseAlternative [NullBinder] (Right mkNothing)
                       ])
          (App e (mkPrelVar "unit"))

  fromSpineFun e _ = App (mkGenVar C.fromSpine) (App e (mkPrelVar "unit"))

  mkRecCase rs = CaseAlternative [ recordBinder [ ArrayBinder (map (VarBinder . Ident . fst) rs)
                                                ]
                                 ]
                   . Right
                   $ liftApplicative (mkRecFun rs) (map (\(x, y) -> fromSpineFun (Accessor "recValue" (mkVar x)) y) rs)

  mkRecFun :: [(String, Type)] -> Expr
  mkRecFun xs = mkJust $ foldr (\s e -> lam s e) recLiteral (map fst xs)
     where recLiteral = ObjectLiteral $ map (\(s,_) -> (s,mkVar s)) xs
mkFromSpineFunction mn (PositionedDeclaration _ _ d) = mkFromSpineFunction mn d
mkFromSpineFunction _ _ = internalError "mkFromSpineFunction: expected DataDeclaration"

-- Helpers

objectType :: Type -> Maybe Type
objectType (TypeApp (TypeConstructor (Qualified (Just (ModuleName [ProperName "Prim"])) (ProperName "Object"))) rec) = Just rec
objectType _ = Nothing

lam :: String -> Expr -> Expr
lam s = Abs (Left (Ident s))

lamNull :: Expr -> Expr
lamNull = lam "$q"

lamCase :: String -> [CaseAlternative] -> Expr
lamCase s = lam s . Case [mkVar s]

liftApplicative :: Expr -> [Expr] -> Expr
liftApplicative = foldl' (\x e -> App (App (mkPrelVar "apply") x) e)

mkVarMn :: Maybe ModuleName -> String -> Expr
mkVarMn mn s = Var (Qualified mn (Ident s))

mkVar :: String -> Expr
mkVar s = mkVarMn Nothing s

mkPrelVar :: String -> Expr
mkPrelVar s = mkVarMn (Just (ModuleName [ProperName C.prelude])) s

mkGenVar :: String -> Expr
mkGenVar s = mkVarMn (Just (ModuleName [ProperName "Data", ProperName C.generic])) s

decomposeRec :: Type -> [(String, Type)]
decomposeRec = sortBy (comparing fst) . go
    where go (RCons str typ typs) = (str, typ) : decomposeRec typs
          go _ = []
