-- |
-- This module implements the desugaring pass which reapplies binary operators based
-- on their fixity data and removes explicit parentheses.
--
-- The value parser ignores fixity data when parsing binary operator applications, so
-- it is necessary to reorder them here.
--
module Language.PureScript.Sugar.Operators
  ( desugarSignedLiterals
  , rebracket
  , checkFixityExports
  ) where

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

import Control.Monad (unless, (<=<))
import Control.Monad.Error.Class (MonadError(..))

import Data.Either (partitionEithers)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Traversable (for)
import qualified Data.Map as M

import qualified Language.PureScript.Constants as C

-- |
-- Removes unary negation operators and replaces them with calls to `negate`.
--
desugarSignedLiterals :: Module a b -> Module a b
desugarSignedLiterals (Module ss coms mn ds exts) =
  Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id
  go (UnaryMinus ann val) = App ann (Var ann (Qualified Nothing (Ident C.negate))) val
  go other = other

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
--
type FixityRecord op alias ann = (Qualified op, SourceSpan, Fixity, Qualified alias, ann)
type ValueFixityRecord ann = FixityRecord (OpName 'ValueOpName) (Either Ident (ProperName 'ConstructorName)) ann
type TypeFixityRecord ann = FixityRecord (OpName 'TypeOpName) (ProperName 'TypeName) ann

-- |
-- Remove explicit parentheses and reorder binary operator applications.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
rebracket
  :: forall m
   . MonadError MultipleErrors m
  => [ExternsFile]
  -> [Module () ()]
  -> m [Module () ()]
rebracket externs modules = do
  let (valueFixities, typeFixities) =
        partitionEithers
          $ concatMap externsFixities externs
          ++ concatMap collectFixities modules

  ensureNoDuplicates' MultipleValueOpFixities valueFixities
  ensureNoDuplicates' MultipleTypeOpFixities typeFixities

  let valueOpTable = customOperatorTable' valueFixities
  let valueAliased = M.fromList (map makeLookupEntry valueFixities)
  let typeOpTable = customOperatorTable' typeFixities
  let typeAliased = M.fromList (map makeLookupEntry typeFixities)

  for modules
    $ renameAliasedOperators valueAliased typeAliased
    <=< rebracketModule valueOpTable typeOpTable

  where

  ensureNoDuplicates'
    :: Ord op
    => (op -> SimpleErrorMessage)
    -> [FixityRecord op alias ann]
    -> m ()
  ensureNoDuplicates' toError =
    ensureNoDuplicates toError . map (\(i, pos, _, _, _) -> (i, pos))

  customOperatorTable'
    :: [FixityRecord op alias ann]
    -> [[(Qualified op, ann, Associativity)]]
  customOperatorTable' =
    customOperatorTable . map (\(i, _, f, _, ann) -> (i, ann, f))

  makeLookupEntry :: FixityRecord op alias ann -> (Qualified op, Qualified alias)
  makeLookupEntry (qname, _, _, alias, _) = (qname, alias)

renameAliasedOperators
  :: forall m a b
   . MonadError MultipleErrors m
  => M.Map (Qualified (OpName 'ValueOpName)) (Qualified (Either Ident (ProperName 'ConstructorName)))
  -> M.Map (Qualified (OpName 'TypeOpName)) (Qualified (ProperName 'TypeName))
  -> Module a b
  -> m (Module a b)
renameAliasedOperators valueAliased typeAliased (Module ss coms mn ds exts) =
  Module ss coms mn <$> mapM f' ds <*> pure exts
  where
  (goDecl', goExpr', goBinder') = updateTypes goType
  (f', _, _, _, _) =
    everywhereWithContextOnValuesM
      Nothing
      (\pos -> uncurry goDecl <=< goDecl' pos)
      (\pos -> uncurry goExpr <=< goExpr' pos)
      (\pos -> uncurry goBinder <=< goBinder' pos)
      defS
      defS

  goDecl :: Maybe SourceSpan -> Declaration a b -> m (Maybe SourceSpan, Declaration a b)
  goDecl _ d@(PositionedDeclaration _ pos _ _) = return (Just pos, d)
  goDecl pos other = return (pos, other)

  goExpr :: Maybe SourceSpan -> Expr a b -> m (Maybe SourceSpan, Expr a b)
  goExpr _ e@(PositionedValue _ pos _ _) = return (Just pos, e)
  goExpr pos (Op ann op) =
    (pos, ) <$> case op `M.lookup` valueAliased of
      Just (Qualified mn' (Left alias)) ->
        return $ Var ann (Qualified mn' alias)
      Just (Qualified mn' (Right alias)) ->
        return $ Constructor ann (Qualified mn' alias)
      Nothing ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage . UnknownName $ fmap ValOpName op
  goExpr pos other = return (pos, other)

  goBinder :: Maybe SourceSpan -> Binder a b -> m (Maybe SourceSpan, Binder a b)
  goBinder _ b@(PositionedBinder _ pos _ _) = return (Just pos, b)
  goBinder pos (BinaryNoParensBinder _ (OpBinder ann op) lhs rhs) =
    case op `M.lookup` valueAliased of
      Just (Qualified mn' (Left alias)) ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage $
            InvalidOperatorInBinder op (Qualified mn' alias)
      Just (Qualified mn' (Right alias)) ->
        return (pos, ConstructorBinder ann (Qualified mn' alias) [lhs, rhs])
      Nothing ->
        maybe id rethrowWithPosition pos $
          throwError . errorMessage . UnknownName $ fmap ValOpName op
  goBinder _ BinaryNoParensBinder{} =
    internalError "BinaryNoParensBinder has no OpBinder"
  goBinder pos other = return (pos, other)

  goType :: Maybe SourceSpan -> Type a -> m (Type a)
  goType pos = maybe id rethrowWithPosition pos . everywhereOnTypesM go
    where
    go :: Type a -> m (Type a)
    go (BinaryNoParensType ann1 (TypeOp ann2 op) lhs rhs) =
      case op `M.lookup` typeAliased of
        Just alias ->
          return $ TypeApp ann1 (TypeApp ann1 (TypeConstructor ann2 alias) lhs) rhs
        Nothing ->
          throwError . errorMessage $ UnknownName $ fmap TyOpName op
    go other = return other

rebracketModule
  :: forall m a b
   . (MonadError MultipleErrors m, Show a, Show b)
  => [[(Qualified (OpName 'ValueOpName), b, Associativity)]]
  -> [[(Qualified (OpName 'TypeOpName), a, Associativity)]]
  -> Module a b
  -> m (Module a b)
rebracketModule valueOpTable typeOpTable (Module ss coms mn ds exts) =
  Module ss coms mn <$> (map removeParens <$> parU ds f) <*> pure exts
  where
  (f, _, _) =
      everywhereOnValuesTopDownM
        (decontextify goDecl)
        (goExpr <=< decontextify goExpr')
        (goBinder <=< decontextify goBinder')

  (goDecl, goExpr', goBinder') = updateTypes (const goType)

  goExpr :: Expr a b -> m (Expr a b)
  goExpr = return . matchExprOperators valueOpTable

  goBinder :: Binder a b -> m (Binder a b)
  goBinder = return . matchBinderOperators valueOpTable

  goType :: Type a -> m (Type a)
  goType = return . matchTypeOperators typeOpTable

  decontextify :: (Maybe SourceSpan -> x -> m (Maybe SourceSpan, x)) -> x -> m x
  decontextify ctxf = fmap snd . ctxf Nothing

removeParens :: Declaration a b -> Declaration a b
removeParens = f
  where
  (f, _, _) =
      everywhereOnValues
        (decontextify goDecl)
        (goExpr . decontextify goExpr')
        (goBinder . decontextify goBinder')

  (goDecl, goExpr', goBinder') = updateTypes (\_ -> return . goType)

  goExpr :: Expr a b -> Expr a b
  goExpr (Parens _ val) = val
  goExpr val = val

  goBinder :: Binder a b -> Binder a b
  goBinder (ParensInBinder _ b) = b
  goBinder b = b

  goType :: Type a -> Type a
  goType (ParensInType _ t) = t
  goType t = t

  decontextify
    :: (Maybe SourceSpan -> x -> Identity (Maybe SourceSpan, x))
    -> x
    -> x
  decontextify ctxf = snd . runIdentity . ctxf Nothing

externsFixities :: ExternsFile -> [Either (ValueFixityRecord ()) (TypeFixityRecord ())]
externsFixities ExternsFile{..} =
  map fromFixity efFixities ++ map fromTypeFixity efTypeFixities
  where

  fromFixity
    :: ExternsFixity
    -> Either (ValueFixityRecord ()) (TypeFixityRecord ())
  fromFixity (ExternsFixity assoc prec op name) =
    Left
      ( Qualified (Just efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      , ()
      )

  fromTypeFixity
    :: ExternsTypeFixity
    -> Either (ValueFixityRecord ()) (TypeFixityRecord ())
  fromTypeFixity (ExternsTypeFixity assoc prec op name) =
    Right
      ( Qualified (Just efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      , ()
      )

collectFixities :: Module a b -> [Either (ValueFixityRecord b) (TypeFixityRecord b)]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration a b -> [Either (ValueFixityRecord b) (TypeFixityRecord b)]
  collect (PositionedDeclaration _ pos _ (ValueFixityDeclaration ann fixity name op)) =
    [Left (Qualified (Just moduleName) op, pos, fixity, name, ann)]
  collect (PositionedDeclaration _ pos _ (TypeFixityDeclaration ann fixity name op)) =
    [Right (Qualified (Just moduleName) op, pos, fixity, name, ann)]
  collect FixityDeclaration{} = internalError "Fixity without srcpos info"
  collect _ = []

ensureNoDuplicates
  :: (Ord a, MonadError MultipleErrors m)
  => (a -> SimpleErrorMessage)
  -> [(Qualified a, SourceSpan)]
  -> m ()
ensureNoDuplicates toError m = go $ sortBy (compare `on` fst) m
  where
  go [] = return ()
  go [_] = return ()
  go ((x@(Qualified (Just mn) op), _) : (y, pos) : _) | x == y =
    rethrow (addHint (ErrorInModule mn)) $
      rethrowWithPosition pos $ throwError . errorMessage $ toError op
  go (_ : rest) = go rest

customOperatorTable
  :: [(Qualified op, ann, Fixity)]
  -> [[(Qualified op, ann, Associativity)]]
customOperatorTable fixities =
  let
    userOps = map (\(name, ann, Fixity a p) -> (name, ann, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, _, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, _, p, _) -> p)) sorted
  in
    map (map (\(name, ann, _, a) -> (name, ann, a))) groups

updateTypes
  :: forall m a b
   . Monad m
  => (Maybe SourceSpan -> Type a -> m (Type a))
  -> ( Maybe SourceSpan -> Declaration a b -> m (Maybe SourceSpan, Declaration a b)
     , Maybe SourceSpan -> Expr a b        -> m (Maybe SourceSpan, Expr a b)
     , Maybe SourceSpan -> Binder a b      -> m (Maybe SourceSpan, Binder a b)
     )
updateTypes goType = (goDecl, goExpr, goBinder)
  where

  goType' :: Maybe SourceSpan -> Type a -> m (Type a)
  goType' = everywhereOnTypesM . goType

  goDecl :: Maybe SourceSpan -> Declaration a b -> m (Maybe SourceSpan, Declaration a b)
  goDecl _ d@(PositionedDeclaration _ pos _ _) = return (Just pos, d)
  goDecl pos (DataDeclaration ann ddt name args dctors) = do
    dctors' <- traverse (sndM (traverse (goType' pos))) dctors
    return (pos, DataDeclaration ann ddt name args dctors')
  goDecl pos (ExternDeclaration ann name ty) = do
    ty' <- goType' pos ty
    return (pos, ExternDeclaration ann name ty')
  goDecl pos (TypeClassDeclaration ann name args implies deps decls) = do
    implies' <- traverse (overConstraintArgs (traverse (goType' pos))) implies
    return (pos, TypeClassDeclaration ann name args implies' deps decls)
  goDecl pos (TypeInstanceDeclaration ann name cs className tys impls) = do
    cs' <- traverse (overConstraintArgs (traverse (goType' pos))) cs
    tys' <- traverse (goType' pos) tys
    return (pos, TypeInstanceDeclaration ann name cs' className tys' impls)
  goDecl pos (TypeSynonymDeclaration ann name args ty) = do
    ty' <- goType' pos ty
    return (pos, TypeSynonymDeclaration ann name args ty')
  goDecl pos (TypeDeclaration ann expr ty) = do
    ty' <- goType' pos ty
    return (pos, TypeDeclaration ann expr ty')
  goDecl pos other = return (pos, other)

  goExpr :: Maybe SourceSpan -> Expr a b -> m (Maybe SourceSpan, Expr a b)
  goExpr _ e@(PositionedValue _ pos _ _) = return (Just pos, e)
  goExpr pos (TypeClassDictionary ann (Constraint name tys info) dicts hints) = do
    tys' <- traverse (goType' pos) tys
    return (pos, TypeClassDictionary ann (Constraint name tys' info) dicts hints)
  goExpr pos (DeferredDictionary ann cls tys) = do
    tys' <- traverse (goType' pos) tys
    return (pos, DeferredDictionary ann cls tys')
  goExpr pos (TypedValue ann check v ty) = do
    ty' <- goType' pos ty
    return (pos, TypedValue ann check v ty')
  goExpr pos other = return (pos, other)

  goBinder :: Maybe SourceSpan -> Binder a b -> m (Maybe SourceSpan, Binder a b)
  goBinder _ e@(PositionedBinder _ pos _ _) = return (Just pos, e)
  goBinder pos (TypedBinder ann ty b) = do
    ty' <- goType' pos ty
    return (pos, TypedBinder ann ty' b)
  goBinder pos other = return (pos, other)

-- |
-- Checks all the fixity exports within a module to ensure that members aliased
-- by the operators are also exported from the module.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
checkFixityExports
  :: forall m a b
   . MonadError MultipleErrors m
  => Module a b
  -> m (Module a b)
checkFixityExports (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before checkFixityExports"
checkFixityExports m@(Module ss _ mn ds (Just exps)) =
  rethrow (addHint (ErrorInModule mn))
    $ rethrowWithPosition ss (traverse_ checkRef exps)
    *> return m
  where

  checkRef :: DeclarationRef -> m ()
  checkRef (PositionedDeclarationRef pos _ d) =
    rethrowWithPosition pos $ checkRef d
  checkRef dr@(ValueOpRef op) =
    for_ (getValueOpAlias op) $ \case
      Left ident ->
        unless (ValueRef ident `elem` exps)
          . throwError . errorMessage
          $ TransitiveExportError dr [ValueRef ident]
      Right ctor ->
        unless (anyTypeRef (maybe False (elem ctor) . snd))
          . throwError . errorMessage
          $ TransitiveDctorExportError dr ctor
  checkRef dr@(TypeOpRef op) =
    for_ (getTypeOpAlias op) $ \ty ->
      unless (anyTypeRef ((== ty) . fst))
        . throwError . errorMessage
        $ TransitiveExportError dr [TypeRef ty Nothing]
  checkRef _ = return ()

  -- Finds the name associated with a type operator when that type is also
  -- defined in the current module.
  getTypeOpAlias :: OpName 'TypeOpName -> Maybe (ProperName 'TypeName)
  getTypeOpAlias op =
    listToMaybe (mapMaybe (either (const Nothing) go <=< getFixityDecl) ds)
    where
    go (TypeFixity _ (Qualified (Just mn') ident) op')
      | mn == mn' && op == op' = Just ident
    go _ = Nothing

  -- Finds the value or data constructor associated with an operator when that
  -- declaration is also in the current module.
  getValueOpAlias
    :: OpName 'ValueOpName
    -> Maybe (Either Ident (ProperName 'ConstructorName))
  getValueOpAlias op =
    listToMaybe (mapMaybe (either go (const Nothing) <=< getFixityDecl) ds)
    where
    go (ValueFixity _ (Qualified (Just mn') ident) op')
      | mn == mn' && op == op' = Just ident
    go _ = Nothing

  -- Tests the exported `TypeRef` entries with a predicate.
  anyTypeRef
    :: ((ProperName 'TypeName, Maybe [ProperName 'ConstructorName]) -> Bool)
    -> Bool
  anyTypeRef f = any (maybe False f . getTypeRef) exps
