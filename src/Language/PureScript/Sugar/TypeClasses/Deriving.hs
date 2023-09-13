-- | This module implements the generic deriving elaboration that takes place during desugaring.
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import Prelude
import Protolude (note)

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class (MonadSupply)
import Data.List (foldl', find, unzip5)
import Language.PureScript.AST (Binder(..), CaseAlternative(..), DataConstructorDeclaration(..), Declaration(..), Expr(..), pattern MkUnguarded, Module(..), SourceSpan(..), TypeInstanceBody(..), pattern ValueDecl)
import Language.PureScript.AST.Utils (UnwrappedTypeConstructor(..), lamCase, unguarded, unwrapTypeConstructor)
import Language.PureScript.Constants.Libs qualified as Libs
import Language.PureScript.Crash (internalError)
import Language.PureScript.Environment (DataDeclType(..), NameKind(..))
import Language.PureScript.Errors (MultipleErrors, SimpleErrorMessage(..), errorMessage')
import Language.PureScript.Names (pattern ByNullSourcePos, Ident(..), ModuleName, ProperName(..), ProperNameType(..), Qualified(..), QualifiedBy(..), freshIdent)
import Language.PureScript.PSString (mkString)
import Language.PureScript.Types (SourceType, Type(..), WildcardData(..), replaceAllTypeVars, srcTypeApp, srcTypeConstructor, srcTypeLevelString)
import Language.PureScript.TypeChecker (checkNewtype)

-- | Elaborates deriving instance declarations by code generation.
deriveInstances
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => Module
  -> m Module
deriveInstances (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM (deriveInstance mn ds) ds <*> pure exts

-- | Takes a declaration, and if the declaration is a deriving TypeInstanceDeclaration,
-- elaborates that into an instance declaration via code generation.
--
-- More instance deriving happens during type checking. The instances
-- derived here are special for two reasons:
-- * they depend only on the structure of the data, not types; and
-- * they expect wildcard types from the user and generate type expressions
--   to replace them.
--
deriveInstance
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => ModuleName
  -> [Declaration]
  -> Declaration
  -> m Declaration
deriveInstance mn ds decl =
  case decl of
    TypeInstanceDeclaration sa@(ss, _) na ch idx nm deps className tys DerivedInstance -> let
      binaryWildcardClass :: (Declaration -> [SourceType] -> m ([Declaration], SourceType)) -> m Declaration
      binaryWildcardClass f = case tys of
        [ty1, ty2] -> case unwrapTypeConstructor ty1 of
          Just UnwrappedTypeConstructor{..} | mn == utcModuleName -> do
            checkIsWildcard ss utcTyCon ty2
            tyConDecl <- findTypeDecl ss utcTyCon ds
            (members, ty2') <- f tyConDecl utcArgs
            pure $ TypeInstanceDeclaration sa na ch idx nm deps className [ty1, ty2'] (ExplicitInstance members)
          _ -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty1
        _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 2

      in case className of
        Libs.Generic -> binaryWildcardClass (deriveGenericRep ss mn)
        Libs.Newtype -> binaryWildcardClass deriveNewtype
        _ -> pure decl
    _ -> pure decl

deriveGenericRep
  :: forall m
   . (MonadError MultipleErrors m, MonadSupply m)
  => SourceSpan
  -> ModuleName
  -> Declaration
  -> [SourceType]
  -> m ([Declaration], SourceType)
deriveGenericRep ss mn tyCon tyConArgs =
  case tyCon of
    DataDeclaration (ss', _) _ _ args dctors -> do
      x <- freshIdent "x"
      (reps, to, from) <- unzip3 <$> traverse makeInst dctors
      let rep = toRepTy reps
          inst | null reps =
                   -- If there are no cases, spin
                   [ ValueDecl (ss', []) (Ident "to") Public [] $ unguarded $
                      lamCase x
                        [ CaseAlternative
                            [NullBinder]
                            (unguarded (App (Var ss Libs.I_to) (Var ss' (Qualified ByNullSourcePos x))))
                        ]
                   , ValueDecl (ss', []) (Ident "from") Public [] $ unguarded $
                      lamCase x
                        [ CaseAlternative
                            [NullBinder]
                            (unguarded (App (Var ss Libs.I_from) (Var ss' (Qualified ByNullSourcePos x))))
                        ]
                   ]
               | otherwise =
                   [ ValueDecl (ss', []) (Ident "to") Public [] $ unguarded $
                       lamCase x (zipWith ($) (map underBinder (sumBinders (length dctors))) to)
                   , ValueDecl (ss', []) (Ident "from") Public [] $ unguarded $
                       lamCase x (zipWith ($) (map underExpr (sumExprs (length dctors))) from)
                   ]

          subst = zipWith ((,) . fst) args tyConArgs
      return (inst, replaceAllTypeVars subst rep)
    _ -> internalError "deriveGenericRep: expected DataDeclaration"

    where

    select :: (a -> a) -> (a -> a) -> Int -> [a -> a]
    select _ _ 0 = []
    select _ _ 1 = [id]
    select l r n = take (n - 1) (iterate (r .) l) ++ [compN (n - 1) r]

    sumBinders :: Int -> [Binder -> Binder]
    sumBinders = select (ConstructorBinder ss Libs.C_Inl . pure)
                        (ConstructorBinder ss Libs.C_Inr . pure)

    sumExprs :: Int -> [Expr -> Expr]
    sumExprs = select (App (Constructor ss Libs.C_Inl))
                      (App (Constructor ss Libs.C_Inr))

    compN :: Int -> (a -> a) -> a -> a
    compN 0 _ = id
    compN n f = f . compN (n - 1) f

    makeInst
      :: DataConstructorDeclaration
      -> m (SourceType, CaseAlternative, CaseAlternative)
    makeInst (DataConstructorDeclaration _ ctorName args) = do
        let args' = map snd args
        (ctorTy, matchProduct, ctorArgs, matchCtor, mkProduct) <- makeProduct args'
        return ( srcTypeApp (srcTypeApp (srcTypeConstructor Libs.Constructor)
                                  (srcTypeLevelString $ mkString (runProperName ctorName)))
                         ctorTy
               , CaseAlternative [ ConstructorBinder ss Libs.C_Constructor [matchProduct] ]
                                 (unguarded (foldl' App (Constructor ss (Qualified (ByModuleName mn) ctorName)) ctorArgs))
               , CaseAlternative [ ConstructorBinder ss (Qualified (ByModuleName mn) ctorName) matchCtor ]
                                 (unguarded (App (Constructor ss Libs.C_Constructor) mkProduct))
               )

    makeProduct
      :: [SourceType]
      -> m (SourceType, Binder, [Expr], [Binder], Expr)
    makeProduct [] =
      pure (srcTypeConstructor Libs.NoArguments, NullBinder, [], [], Constructor ss Libs.C_NoArguments)
    makeProduct args = do
      (tys, bs1, es1, bs2, es2) <- unzip5 <$> traverse makeArg args
      pure ( foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor Libs.Product) f)) tys
           , foldr1 (\b1 b2 -> ConstructorBinder ss Libs.C_Product [b1, b2]) bs1
           , es1
           , bs2
           , foldr1 (\e1 -> App (App (Constructor ss Libs.C_Product) e1)) es2
           )

    makeArg :: SourceType -> m (SourceType, Binder, Expr, Binder, Expr)
    makeArg arg = do
      argName <- freshIdent "arg"
      pure ( srcTypeApp (srcTypeConstructor Libs.Argument) arg
           , ConstructorBinder ss Libs.C_Argument [ VarBinder ss argName ]
           , Var ss (Qualified (BySourcePos $ spanStart ss) argName)
           , VarBinder ss argName
           , App (Constructor ss Libs.C_Argument) (Var ss (Qualified (BySourcePos $ spanStart ss) argName))
           )

    underBinder :: (Binder -> Binder) -> CaseAlternative -> CaseAlternative
    underBinder f (CaseAlternative bs e) = CaseAlternative (map f bs) e

    underExpr :: (Expr -> Expr) -> CaseAlternative -> CaseAlternative
    underExpr f (CaseAlternative b [MkUnguarded e]) = CaseAlternative b (unguarded (f e))
    underExpr _ _ = internalError "underExpr: expected unguarded alternative"

    toRepTy :: [SourceType] -> SourceType
    toRepTy [] = srcTypeConstructor Libs.NoConstructors
    toRepTy [only] = only
    toRepTy ctors = foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor Libs.Sum) f)) ctors

checkIsWildcard :: MonadError MultipleErrors m => SourceSpan -> ProperName 'TypeName -> SourceType -> m ()
checkIsWildcard _ _ (TypeWildcard _ UnnamedWildcard) = return ()
checkIsWildcard ss tyConNm _ =
  throwError . errorMessage' ss $ ExpectedWildcard tyConNm

deriveNewtype
  :: forall m
   . MonadError MultipleErrors m
  => Declaration
  -> [SourceType]
  -> m ([Declaration], SourceType)
deriveNewtype tyCon tyConArgs =
  case tyCon of
    DataDeclaration (ss', _) Data name _ _ ->
      throwError . errorMessage' ss' $ CannotDeriveNewtypeForData name
    DataDeclaration _ Newtype name args dctors -> do
      (_, (_, ty)) <- checkNewtype name dctors
      let subst = zipWith ((,) . fst) args tyConArgs
      return ([], replaceAllTypeVars subst ty)
    _ -> internalError "deriveNewtype: expected DataDeclaration"

findTypeDecl
  :: (MonadError MultipleErrors m)
  => SourceSpan
  -> ProperName 'TypeName
  -> [Declaration]
  -> m Declaration
findTypeDecl ss tyConNm = note (errorMessage' ss $ CannotFindDerivingType tyConNm) . find isTypeDecl
  where
  isTypeDecl :: Declaration -> Bool
  isTypeDecl (DataDeclaration _ _ nm _ _) = nm == tyConNm
  isTypeDecl _ = False
