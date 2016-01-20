{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- This module implements the generic deriving elaboration that takes place during desugaring.
--
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import Prelude ()
import Prelude.Compat

import Data.List (foldl', find, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import Control.Arrow (second)
import Control.Monad (replicateM)
import Control.Monad.Supply.Class (MonadSupply)
import Control.Monad.Error.Class (MonadError(..))

import Language.PureScript.Crash
import Language.PureScript.AST
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Types
import qualified Language.PureScript.Constants as C

-- | Elaborates deriving instance declarations by code generation.
deriveInstances
  :: (Functor m, Applicative m, MonadError MultipleErrors m, MonadSupply m)
  => Module
  -> m Module
deriveInstances (Module ss coms mn ds exts) = Module ss coms mn <$> mapM (deriveInstance mn ds) ds <*> pure exts

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
deriveInstance
  :: (Functor m, MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> Declaration
  -> m Declaration
deriveInstance mn ds (TypeInstanceDeclaration nm deps className tys@[ty] DerivedInstance)
  | className == Qualified (Just dataGeneric) (ProperName C.generic)
  , Just (Qualified mn' tyCon, args) <- unwrapTypeConstructor ty
  , mn == fromMaybe mn mn'
  = TypeInstanceDeclaration nm deps className tys . ExplicitInstance <$> deriveGeneric mn ds tyCon args
deriveInstance _ _ (TypeInstanceDeclaration _ _ className tys DerivedInstance)
  = throwError . errorMessage $ CannotDerive className tys
deriveInstance mn ds (PositionedDeclaration pos com d) = PositionedDeclaration pos com <$> deriveInstance mn ds d
deriveInstance _  _  e = return e

unwrapTypeConstructor :: Type -> Maybe (Qualified (ProperName 'TypeName), [Type])
unwrapTypeConstructor = fmap (second reverse) . go
  where
  go (TypeConstructor tyCon) = Just (tyCon, [])
  go (TypeApp ty arg) = do
    (tyCon, args) <- go ty
    return (tyCon, arg : args)
  go _ = Nothing

dataGeneric :: ModuleName
dataGeneric = ModuleName [ ProperName "Data", ProperName "Generic" ]

dataMaybe :: ModuleName
dataMaybe = ModuleName [ ProperName "Data", ProperName "Maybe" ]

typesProxy :: ModuleName
typesProxy = ModuleName [ ProperName "Type", ProperName "Proxy" ]

deriveGeneric
  :: (Functor m, MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> ProperName 'TypeName
  -> [Type]
  -> m [Declaration]
deriveGeneric mn ds tyConNm args = do
  tyCon <- findTypeDecl tyConNm ds
  toSpine <- mkSpineFunction mn tyCon
  fromSpine <- mkFromSpineFunction mn tyCon
  let toSignature = mkSignatureFunction mn tyCon args
  return [ ValueDeclaration (Ident C.toSpine) Public [] (Right toSpine)
         , ValueDeclaration (Ident C.fromSpine) Public [] (Right fromSpine)
         , ValueDeclaration (Ident C.toSignature) Public [] (Right toSignature)
         ]

findTypeDecl
  :: (Functor m, MonadError MultipleErrors m)
  => ProperName 'TypeName
  -> [Declaration]
  -> m Declaration
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

  mkCtorClause :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
  mkCtorClause (ctorName, tys) = do
    idents <- replicateM (length tys) freshIdent'
    return $ CaseAlternative [ConstructorBinder (Qualified (Just mn) ctorName) (map VarBinder idents)] (Right (caseResult idents))
    where
    caseResult idents =
      App (prodConstructor (StringLiteral . showQualified runProperName $ Qualified (Just mn) ctorName))
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

mkSignatureFunction :: ModuleName -> Declaration -> [Type] -> Expr
mkSignatureFunction mn (DataDeclaration _ name tyArgs args) classArgs = lamNull . mkSigProd $ map mkProdClause args
  where
  mkSigProd :: [Expr] -> Expr
  mkSigProd = App (App (Constructor (Qualified (Just dataGeneric) (ProperName "SigProd")))
                       (StringLiteral (showQualified runProperName (Qualified (Just mn) name)))
                  ) . ArrayLiteral

  mkSigRec :: [Expr] -> Expr
  mkSigRec = App (Constructor (Qualified (Just dataGeneric) (ProperName "SigRecord"))) . ArrayLiteral

  proxy :: Type -> Type
  proxy = TypeApp (TypeConstructor (Qualified (Just typesProxy) (ProperName "Proxy")))

  mkProdClause :: (ProperName 'ConstructorName, [Type]) -> Expr
  mkProdClause (ctorName, tys) =
    ObjectLiteral
      [ ("sigConstructor", StringLiteral (showQualified runProperName (Qualified (Just mn) ctorName)))
      , ("sigValues", ArrayLiteral . map (mkProductSignature . instantiate) $ tys)
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
  instantiate = replaceAllTypeVars (zipWith (\(arg, _) ty -> (arg, ty)) tyArgs classArgs)
mkSignatureFunction mn (PositionedDeclaration _ _ d) classArgs = mkSignatureFunction mn d classArgs
mkSignatureFunction _ _ _ = internalError "mkSignatureFunction: expected DataDeclaration"

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

  mkAlternative :: (ProperName 'ConstructorName, [Type]) -> m CaseAlternative
  mkAlternative (ctorName, tys) = do
    idents <- replicateM (length tys) freshIdent'
    return $ CaseAlternative [ prodBinder [ StringBinder (showQualified runProperName (Qualified (Just mn) ctorName)), ArrayBinder (map VarBinder idents)]]
               . Right
               $ liftApplicative (mkJust $ Constructor (Qualified (Just mn) ctorName))
                                 (zipWith fromSpineFun (map (Var . Qualified Nothing) idents) tys)

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
  mkRecFun xs = mkJust $ foldr lam recLiteral (map fst xs)
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
mkVar = mkVarMn Nothing

mkPrelVar :: String -> Expr
mkPrelVar = mkVarMn (Just (ModuleName [ProperName C.prelude]))

mkGenVar :: String -> Expr
mkGenVar = mkVarMn (Just (ModuleName [ProperName "Data", ProperName C.generic]))

decomposeRec :: Type -> [(String, Type)]
decomposeRec = sortBy (comparing fst) . go
    where go (RCons str typ typs) = (str, typ) : decomposeRec typs
          go _ = []
