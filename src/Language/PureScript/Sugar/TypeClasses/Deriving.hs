-- | This module implements the generic deriving elaboration that takes place during desugaring.
module Language.PureScript.Sugar.TypeClasses.Deriving (deriveInstances) where

import           Prelude
import           Protolude (note)

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class (MonadSupply)
import           Data.List (foldl', find, unzip5)
import           Language.PureScript.AST
import           Language.PureScript.AST.Utils
import qualified Language.PureScript.Constants.Data.Generic.Rep as DataGenericRep
import qualified Language.PureScript.Constants.Data.Newtype as DataNewtype
import           Language.PureScript.Crash
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Names
import           Language.PureScript.PSString (mkString)
import           Language.PureScript.Types
import           Language.PureScript.TypeChecker (checkNewtype)

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
          Just (Qualified (ByModuleName mn') tyCon, _, args) | mn == mn' -> do
            checkIsWildcard ss tyCon ty2
            tyConDecl <- findTypeDecl ss tyCon ds
            (members, ty2') <- f tyConDecl args
            pure $ TypeInstanceDeclaration sa na ch idx nm deps className [ty1, ty2'] (ExplicitInstance members)
          _ -> throwError . errorMessage' ss $ ExpectedTypeConstructor className tys ty1
        _ -> throwError . errorMessage' ss $ InvalidDerivedInstance className tys 2

      in case className of
        DataNewtype.Newtype -> binaryWildcardClass deriveNewtype
        DataGenericRep.Generic -> binaryWildcardClass (deriveGenericRep ss mn)
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
                            (unguarded (App (Var ss DataGenericRep.to) (Var ss' (Qualified ByNullSourcePos x))))
                        ]
                   , ValueDecl (ss', []) (Ident "from") Public [] $ unguarded $
                      lamCase x
                        [ CaseAlternative
                            [NullBinder]
                            (unguarded (App (Var ss DataGenericRep.from) (Var ss' (Qualified ByNullSourcePos x))))
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
    sumBinders = select (ConstructorBinder ss DataGenericRep.Inl . pure)
                        (ConstructorBinder ss DataGenericRep.Inr . pure)

    sumExprs :: Int -> [Expr -> Expr]
    sumExprs = select (App (Constructor ss DataGenericRep.Inl))
                      (App (Constructor ss DataGenericRep.Inr))

    compN :: Int -> (a -> a) -> a -> a
    compN 0 _ = id
    compN n f = f . compN (n - 1) f

    makeInst
      :: DataConstructorDeclaration
      -> m (SourceType, CaseAlternative, CaseAlternative)
    makeInst (DataConstructorDeclaration _ ctorName args) = do
        let args' = map snd args
        (ctorTy, matchProduct, ctorArgs, matchCtor, mkProduct) <- makeProduct args'
        return ( srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Constructor)
                                  (srcTypeLevelString $ mkString (runProperName ctorName)))
                         ctorTy
               , CaseAlternative [ ConstructorBinder ss DataGenericRep.Constructor [matchProduct] ]
                                 (unguarded (foldl' App (Constructor ss (Qualified (ByModuleName mn) ctorName)) ctorArgs))
               , CaseAlternative [ ConstructorBinder ss (Qualified (ByModuleName mn) ctorName) matchCtor ]
                                 (unguarded (App (Constructor ss DataGenericRep.Constructor) mkProduct))
               )

    makeProduct
      :: [SourceType]
      -> m (SourceType, Binder, [Expr], [Binder], Expr)
    makeProduct [] =
      pure (srcTypeConstructor DataGenericRep.NoArguments, NullBinder, [], [], Constructor ss DataGenericRep.NoArguments)
    makeProduct args = do
      (tys, bs1, es1, bs2, es2) <- unzip5 <$> traverse makeArg args
      pure ( foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Product) f)) tys
           , foldr1 (\b1 b2 -> ConstructorBinder ss DataGenericRep.Product [b1, b2]) bs1
           , es1
           , bs2
           , foldr1 (\e1 -> App (App (Constructor ss DataGenericRep.Product) e1)) es2
           )

    makeArg :: SourceType -> m (SourceType, Binder, Expr, Binder, Expr)
    makeArg arg = do
      argName <- freshIdent "arg"
      pure ( srcTypeApp (srcTypeConstructor DataGenericRep.Argument) arg
           , ConstructorBinder ss DataGenericRep.Argument [ VarBinder ss argName ]
           , Var ss (Qualified (BySourcePos $ spanStart ss) argName)
           , VarBinder ss argName
           , App (Constructor ss DataGenericRep.Argument) (Var ss (Qualified (BySourcePos $ spanStart ss) argName))
           )

    underBinder :: (Binder -> Binder) -> CaseAlternative -> CaseAlternative
    underBinder f (CaseAlternative bs e) = CaseAlternative (map f bs) e

    underExpr :: (Expr -> Expr) -> CaseAlternative -> CaseAlternative
    underExpr f (CaseAlternative b [MkUnguarded e]) = CaseAlternative b (unguarded (f e))
    underExpr _ _ = internalError "underExpr: expected unguarded alternative"

    toRepTy :: [SourceType] -> SourceType
    toRepTy [] = srcTypeConstructor DataGenericRep.NoConstructors
    toRepTy [only] = only
    toRepTy ctors = foldr1 (\f -> srcTypeApp (srcTypeApp (srcTypeConstructor DataGenericRep.Sum) f)) ctors

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
