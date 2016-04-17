{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
module Language.PureScript.Sugar.Operators
  ( rebracket
  , removeSignedLiterals
  , desugarOperatorSections
  ) where

import Prelude ()
import Prelude.Compat

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Externs
import Language.PureScript.Names
import Language.PureScript.Sugar.Operators.Binders
import Language.PureScript.Sugar.Operators.Expr
import Language.PureScript.Sugar.Operators.Types
import Language.PureScript.Traversals (defS, sndM)
import Language.PureScript.Types

import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Supply.Class

import Data.Function (on)
import Data.Functor.Identity
import Data.List (partition, groupBy, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import qualified Language.PureScript.Constants as C

-- TODO: in 0.9 operators names can have their own type rather than being in a sum with `Ident`, and `FixityAlias` no longer needs to be optional

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
--
type FixityRecord = (Qualified Ident, SourceSpan, Fixity, Maybe (Qualified FixityAlias))

-- |
-- Remove explicit parentheses and reorder binary operator applications
--
rebracket
  :: forall m
   . MonadError MultipleErrors m
  => [ExternsFile]
  -> [Module]
  -> m [Module]
rebracket externs ms = do
  let (typeFixities, valueFixities) = partition isTypeFixity $
        concatMap externsFixities externs ++ concatMap collectFixities ms

  ensureNoDuplicates' $ valueFixities
  ensureNoDuplicates' $ typeFixities

  let valueOpTable = customOperatorTable' valueFixities
      typeOpTable = customOperatorTable' typeFixities
  ms' <- traverse (rebracketModule valueOpTable typeOpTable) ms

  let valueAliased = M.fromList (mapMaybe makeLookupEntry valueFixities)
      typeAliased = M.fromList (mapMaybe makeLookupEntry typeFixities)
  mapM (renameAliasedOperators valueAliased typeAliased) ms'

  where

  isTypeFixity :: FixityRecord -> Bool
  -- Nothing case for FixityAlias can only ever be a value fixity, as it's not
  -- possible to define types with operator names aside through aliasing.
  -- TODO: This comment is redundant after 0.9.
  isTypeFixity (_, _, _, Just (Qualified _ (AliasType _))) = True
  isTypeFixity _ = False

  ensureNoDuplicates' :: [FixityRecord] -> m ()
  ensureNoDuplicates' =
    ensureNoDuplicates . map (\(i, pos, _, _) -> (i, pos))

  customOperatorTable' :: [FixityRecord] -> [[(Qualified Ident, Associativity)]]
  customOperatorTable' =
    customOperatorTable . map (\(i, _, f, _) -> (i, f))

  makeLookupEntry :: FixityRecord -> Maybe (Qualified Ident, Qualified FixityAlias)
  makeLookupEntry (qname, _, _, alias) = (qname, ) <$> alias

  renameAliasedOperators
    :: M.Map (Qualified Ident) (Qualified FixityAlias)
    -> M.Map (Qualified Ident) (Qualified FixityAlias)
    -> Module
    -> m Module
  renameAliasedOperators valueAliased typeAliased (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM f' ds <*> pure exts
    where
    (goDecl', goExpr') = updateTypes goType
    (f', _, _, _, _) =
      everywhereWithContextOnValuesM
        Nothing
        (\pos -> uncurry goDecl <=< goDecl' pos)
        (\pos -> uncurry goExpr <=< goExpr' pos)
        goBinder
        defS
        defS

    goDecl :: Maybe SourceSpan -> Declaration -> m (Maybe SourceSpan, Declaration)
    goDecl _ d@(PositionedDeclaration pos _ _) = return (Just pos, d)
    goDecl pos other = return (pos, other)

    goExpr :: Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
    goExpr _ e@(PositionedValue pos _ _) = return (Just pos, e)
    goExpr pos (Var name) = return (pos, case name `M.lookup` valueAliased of
      Just (Qualified mn' (AliasValue alias)) -> Var (Qualified mn' alias)
      Just (Qualified mn' (AliasConstructor alias)) -> Constructor (Qualified mn' alias)
      _ -> Var name)
    goExpr pos other = return (pos, other)

    goBinder :: Maybe SourceSpan -> Binder -> m (Maybe SourceSpan, Binder)
    goBinder _ b@(PositionedBinder pos _ _) = return (Just pos, b)
    goBinder pos (BinaryNoParensBinder (OpBinder name) lhs rhs) = case name `M.lookup` valueAliased of
      Just (Qualified _ (AliasValue alias)) ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $ InvalidOperatorInBinder (disqualify name) alias
      Just (Qualified mn' (AliasConstructor alias)) ->
        return (pos, ConstructorBinder (Qualified mn' alias) [lhs, rhs])
      _ ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $ UnknownValue name
    goBinder _ (BinaryNoParensBinder {}) =
      internalError "BinaryNoParensBinder has no OpBinder"
    goBinder pos other = return (pos, other)

    goType :: Maybe SourceSpan -> Type -> m Type
    goType pos = everywhereOnTypesM go
      where
      go :: Type -> m Type
      go (BinaryNoParensType (TypeOp name) lhs rhs) = case name `M.lookup` typeAliased of
        Just (Qualified mn' (AliasType alias)) ->
          return $ TypeApp (TypeApp (TypeConstructor (Qualified mn' alias)) lhs) rhs
        _ ->
          maybe id rethrowWithPosition pos $
            throwError . errorMessage $ UnknownTypeOp name
      go other = return other

removeSignedLiterals :: Module -> Module
removeSignedLiterals (Module ss coms mn ds exts) = Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id

  go (UnaryMinus val) = App (Var (Qualified Nothing (Ident C.negate))) val
  go other = other

rebracketModule
  :: forall m
   . (MonadError MultipleErrors m)
  => [[(Qualified Ident, Associativity)]]
  -> [[(Qualified Ident, Associativity)]]
  -> Module
  -> m Module
rebracketModule valueOpTable typeOpTable (Module ss coms mn ds exts) =
  Module ss coms mn <$> (map removeParens <$> parU ds f) <*> pure exts
  where
  (f, _, _) =
      everywhereOnValuesTopDownM
        (decontextify goDecl)
        (goExpr <=< decontextify goExpr')
        goBinder

  (goDecl, goExpr') = updateTypes (\_ -> goType)

  goExpr :: Expr -> m Expr
  goExpr = return . matchExprOperators valueOpTable

  goBinder :: Binder -> m Binder
  goBinder = return . matchBinderOperators valueOpTable

  goType :: Type -> m Type
  goType = return . matchTypeOperators typeOpTable

  decontextify :: (Maybe SourceSpan -> a -> m (Maybe SourceSpan, a)) -> a -> m a
  decontextify ctxf = fmap snd . ctxf Nothing

removeParens :: Declaration -> Declaration
removeParens = f
  where
  (f, _, _) =
      everywhereOnValues
        (decontextify goDecl)
        (goExpr . decontextify goExpr')
        goBinder

  (goDecl, goExpr') = updateTypes (\_ -> return . goType)

  goExpr :: Expr -> Expr
  goExpr (Parens val) = val
  goExpr val = val

  goBinder :: Binder -> Binder
  goBinder (ParensInBinder b) = b
  goBinder b = b

  goType :: Type -> Type
  goType (ParensInType t) = t
  goType t = t

  decontextify
    :: (Maybe SourceSpan -> a -> Identity (Maybe SourceSpan, a))
    -> a
    -> a
  decontextify ctxf = snd . runIdentity . ctxf Nothing

externsFixities
  :: ExternsFile
  -> [FixityRecord]
externsFixities ExternsFile{..} =
   [ (Qualified (Just efModuleName) (Op op), internalModuleSourceSpan "", Fixity assoc prec, alias)
   | ExternsFixity assoc prec op alias <- efFixities
   ]

collectFixities :: Module -> [FixityRecord]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [FixityRecord]
  collect (PositionedDeclaration pos _ (FixityDeclaration fixity name alias)) =
    [(Qualified (Just moduleName) (Op name), pos, fixity, alias)]
  collect FixityDeclaration{} = internalError "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates
  :: MonadError MultipleErrors m
  => [(Qualified Ident, SourceSpan)]
  -> m ()
ensureNoDuplicates m = go $ sortBy (compare `on` fst) m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (Just mn) name), _) : (y, pos) : _) | x == y =
    rethrow (addHint (ErrorInModule mn)) $
      rethrowWithPosition pos $
        throwError . errorMessage $ MultipleFixities name
  go (_ : rest) = go rest

customOperatorTable
  :: [(Qualified Ident, Fixity)]
  -> [[(Qualified Ident, Associativity)]]
customOperatorTable fixities =
  let
    userOps = map (\(name, Fixity a p) -> (name, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, p, _) -> p)) sorted
  in
    map (map (\(name, _, a) -> (name, a))) groups

desugarOperatorSections
  :: forall m
   . (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarOperatorSections (Module ss coms mn ds exts) =
  Module ss coms mn <$> traverse goDecl ds <*> pure exts
  where

  goDecl :: Declaration -> m Declaration
  (goDecl, _, _) = everywhereOnValuesM return goExpr return

  goExpr :: Expr -> m Expr
  goExpr (OperatorSection op eVal) = do
    arg <- freshIdent'
    let var = Var (Qualified Nothing arg)
        f2 a b = Abs (Left arg) $ App (App op a) b
    return $ case eVal of
      Left  val -> f2 val var
      Right val -> f2 var val
  goExpr other = return other

updateTypes
  :: forall m
   . Monad m
  => (Maybe SourceSpan -> Type -> m Type)
  -> ( Maybe SourceSpan -> Declaration -> m (Maybe SourceSpan, Declaration)
     , Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
     )
updateTypes goType = (goDecl, goExpr)
  where

  goType' :: Maybe SourceSpan -> Type -> m Type
  goType' = everywhereOnTypesM . goType

  goDecl :: Maybe SourceSpan -> Declaration -> m (Maybe SourceSpan, Declaration)
  goDecl _ d@(PositionedDeclaration pos _ _) = return (Just pos, d)
  goDecl pos (DataDeclaration ddt name args dctors) = do
    dctors' <- traverse (sndM (traverse (goType' pos))) dctors
    return (pos, DataDeclaration ddt name args dctors')
  goDecl pos (ExternDeclaration name ty) = do
    ty' <- goType' pos ty
    return (pos, ExternDeclaration name ty')
  goDecl pos (TypeClassDeclaration name args implies decls) = do
    implies' <- traverse (sndM (traverse (goType' pos))) implies
    return (pos, TypeClassDeclaration name args implies' decls)
  goDecl pos (TypeInstanceDeclaration name cs className tys impls) = do
    cs' <- traverse (sndM (traverse (goType' pos))) cs
    tys' <- traverse (goType' pos) tys
    return (pos, TypeInstanceDeclaration name cs' className tys' impls)
  goDecl pos (TypeSynonymDeclaration name args ty) = do
    ty' <- goType' pos ty
    return (pos, TypeSynonymDeclaration name args ty')
  goDecl pos (TypeDeclaration expr ty) = do
    ty' <- goType' pos ty
    return (pos, TypeDeclaration expr ty')
  goDecl pos other = return (pos, other)

  goExpr :: Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
  goExpr _ e@(PositionedValue pos _ _) = return (Just pos, e)
  goExpr pos (TypeClassDictionary (name, tys) dicts) = do
    tys' <- traverse (goType' pos) tys
    return (pos, TypeClassDictionary (name, tys') dicts)
  goExpr pos (SuperClassDictionary cls tys) = do
    tys' <- traverse (goType' pos) tys
    return (pos, SuperClassDictionary cls tys')
  goExpr pos (TypedValue check v ty) = do
    ty' <- goType' pos ty
    return (pos, TypedValue check v ty')
  goExpr pos other = return (pos, other)
