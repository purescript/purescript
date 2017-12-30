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
  , rebracketFiltered
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
desugarSignedLiterals :: Module -> Module
desugarSignedLiterals (Module ss coms mn ds exts) =
  Module ss coms mn (map f' ds) exts
  where
  (f', _, _) = everywhereOnValues id go id
  go (UnaryMinus val) = App (Var (Qualified Nothing (Ident C.negate))) val
  go other = other

-- |
-- An operator associated with its declaration position, fixity, and the name
-- of the function or data constructor it is an alias for.
--
type FixityRecord op alias = (Qualified op, SourceSpan, Fixity, Qualified alias)
type ValueFixityRecord = FixityRecord (OpName 'ValueOpName) (Either Ident (ProperName 'ConstructorName))
type TypeFixityRecord = FixityRecord (OpName 'TypeOpName) (ProperName 'TypeName)

-- |
-- Remove explicit parentheses and reorder binary operator applications.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
rebracket
  :: forall m
   . MonadError MultipleErrors m
  => [ExternsFile]
  -> [Module]
  -> m [Module]
rebracket =
  rebracketFiltered (const True)

-- |
-- A version of `rebracket` which allows you to choose which declarations
-- should be affected. This is used in docs generation, where we want to
-- desugar type operators in instance declarations to ensure that instances are
-- paired up with their types correctly, but we don't want to desugar type
-- operators in value declarations.
--
rebracketFiltered
  :: forall m
   . MonadError MultipleErrors m
  => (Declaration -> Bool)
  -> [ExternsFile]
  -> [Module]
  -> m [Module]
rebracketFiltered pred_ externs modules = do
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
    <=< rebracketModule pred_ valueOpTable typeOpTable

  where

  ensureNoDuplicates'
    :: Ord op
    => (op -> SimpleErrorMessage)
    -> [FixityRecord op alias]
    -> m ()
  ensureNoDuplicates' toError =
    ensureNoDuplicates toError . map (\(i, pos, _, _) -> (i, pos))

  customOperatorTable'
    :: [FixityRecord op alias]
    -> [[(Qualified op, Associativity)]]
  customOperatorTable' = customOperatorTable . map (\(i, _, f, _) -> (i, f))

  makeLookupEntry :: FixityRecord op alias -> (Qualified op, Qualified alias)
  makeLookupEntry (qname, _, _, alias) = (qname, alias)

  renameAliasedOperators
    :: M.Map (Qualified (OpName 'ValueOpName)) (Qualified (Either Ident (ProperName 'ConstructorName)))
    -> M.Map (Qualified (OpName 'TypeOpName)) (Qualified (ProperName 'TypeName))
    -> Module
    -> m Module
  renameAliasedOperators valueAliased typeAliased (Module ss coms mn ds exts) =
    Module ss coms mn <$> mapM (usingPredicate pred_ f') ds <*> pure exts
    where
    (goDecl', goExpr', goBinder') = updateTypes goType
    (f', _, _, _, _) =
      everywhereWithContextOnValuesM
        Nothing
        (\_ d -> (Just (declSourceSpan d),) <$> goDecl' d)
        (\pos -> uncurry goExpr <=< goExpr' pos)
        (\pos -> uncurry goBinder <=< goBinder' pos)
        defS
        defS

    goExpr :: Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
    goExpr _ e@(PositionedValue pos _ _) = return (Just pos, e)
    goExpr pos (Op op) =
      (pos, ) <$> case op `M.lookup` valueAliased of
        Just (Qualified mn' (Left alias)) ->
          return $ Var (Qualified mn' alias)
        Just (Qualified mn' (Right alias)) ->
          return $ Constructor (Qualified mn' alias)
        Nothing ->
          maybe id rethrowWithPosition pos $
            throwError . errorMessage . UnknownName $ fmap ValOpName op
    goExpr pos other = return (pos, other)

    goBinder :: Maybe SourceSpan -> Binder -> m (Maybe SourceSpan, Binder)
    goBinder _ b@(PositionedBinder pos _ _) = return (Just pos, b)
    goBinder pos (BinaryNoParensBinder (OpBinder op) lhs rhs) =
      case op `M.lookup` valueAliased of
        Just (Qualified mn' (Left alias)) ->
          maybe id rethrowWithPosition pos $
            throwError . errorMessage $
              InvalidOperatorInBinder op (Qualified mn' alias)
        Just (Qualified mn' (Right alias)) ->
          return (pos, ConstructorBinder (Qualified mn' alias) [lhs, rhs])
        Nothing ->
          maybe id rethrowWithPosition pos $
            throwError . errorMessage . UnknownName $ fmap ValOpName op
    goBinder _ BinaryNoParensBinder{} =
      internalError "BinaryNoParensBinder has no OpBinder"
    goBinder pos other = return (pos, other)

    goType :: Maybe SourceSpan -> Type -> m Type
    goType pos = maybe id rethrowWithPosition pos . go
      where
      go :: Type -> m Type
      go (BinaryNoParensType (TypeOp op) lhs rhs) =
        case op `M.lookup` typeAliased of
          Just alias ->
            return $ TypeApp (TypeApp (TypeConstructor alias) lhs) rhs
          Nothing ->
            throwError . errorMessage $ UnknownName $ fmap TyOpName op
      go other = return other

rebracketModule
  :: forall m
   . (MonadError MultipleErrors m)
  => (Declaration -> Bool)
  -> [[(Qualified (OpName 'ValueOpName), Associativity)]]
  -> [[(Qualified (OpName 'TypeOpName), Associativity)]]
  -> Module
  -> m Module
rebracketModule pred_ valueOpTable typeOpTable (Module ss coms mn ds exts) =
  Module ss coms mn <$> f' ds <*> pure exts
  where
  f' :: [Declaration] -> m [Declaration]
  f' =
    fmap (map (\d -> if pred_ d then removeParens d else d)) .
    flip parU (usingPredicate pred_ f)

  (f, _, _) =
      everywhereOnValuesTopDownM
        goDecl
        (goExpr <=< decontextify goExpr')
        (goBinder <=< decontextify goBinder')

  (goDecl, goExpr', goBinder') = updateTypes (const goType)

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
        (runIdentity . goDecl)
        (goExpr . decontextify goExpr')
        (goBinder . decontextify goBinder')

  (goDecl, goExpr', goBinder') = updateTypes (\_ -> return . goType)

  goExpr :: Expr -> Expr
  goExpr (Parens val) = goExpr val
  goExpr val = val

  goBinder :: Binder -> Binder
  goBinder (ParensInBinder b) = goBinder b
  goBinder b = b

  goType :: Type -> Type
  goType (ParensInType t) = goType t
  goType t = t

  decontextify
    :: (Maybe SourceSpan -> a -> Identity (Maybe SourceSpan, a))
    -> a
    -> a
  decontextify ctxf = snd . runIdentity . ctxf Nothing

externsFixities :: ExternsFile -> [Either ValueFixityRecord TypeFixityRecord]
externsFixities ExternsFile{..} =
  map fromFixity efFixities ++ map fromTypeFixity efTypeFixities
  where

  fromFixity
    :: ExternsFixity
    -> Either ValueFixityRecord TypeFixityRecord
  fromFixity (ExternsFixity assoc prec op name) =
    Left
      ( Qualified (Just efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      )

  fromTypeFixity
    :: ExternsTypeFixity
    -> Either ValueFixityRecord TypeFixityRecord
  fromTypeFixity (ExternsTypeFixity assoc prec op name) =
    Right
      ( Qualified (Just efModuleName) op
      , internalModuleSourceSpan ""
      , Fixity assoc prec
      , name
      )

collectFixities :: Module -> [Either ValueFixityRecord TypeFixityRecord]
collectFixities (Module _ _ moduleName ds _) = concatMap collect ds
  where
  collect :: Declaration -> [Either ValueFixityRecord TypeFixityRecord]
  collect (ValueFixityDeclaration (ss, _) fixity name op) =
    [Left (Qualified (Just moduleName) op, ss, fixity, name)]
  collect (TypeFixityDeclaration (ss, _) fixity name op) =
    [Right (Qualified (Just moduleName) op, ss, fixity, name)]
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
  :: [(Qualified op, Fixity)]
  -> [[(Qualified op, Associativity)]]
customOperatorTable fixities =
  let
    userOps = map (\(name, Fixity a p) -> (name, p, a)) fixities
    sorted = sortBy (flip compare `on` (\(_, p, _) -> p)) userOps
    groups = groupBy ((==) `on` (\(_, p, _) -> p)) sorted
  in
    map (map (\(name, _, a) -> (name, a))) groups

updateTypes
  :: forall m
   . Monad m
  => (Maybe SourceSpan -> Type -> m Type)
  -> ( Declaration -> m Declaration
     , Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
     , Maybe SourceSpan -> Binder -> m (Maybe SourceSpan, Binder)
     )
updateTypes goType = (goDecl, goExpr, goBinder)
  where

  goType' :: Maybe SourceSpan -> Type -> m Type
  goType' = everywhereOnTypesTopDownM . goType

  goType'' :: SourceSpan -> Type -> m Type
  goType'' = goType' . Just

  goDecl :: Declaration -> m Declaration
  goDecl (DataDeclaration sa@(ss, _) ddt name args dctors) =
    DataDeclaration sa ddt name args <$> traverse (sndM (traverse (goType'' ss))) dctors
  goDecl (ExternDeclaration sa@(ss, _) name ty) =
    ExternDeclaration sa name <$> goType'' ss ty
  goDecl (TypeClassDeclaration sa@(ss, _) name args implies deps decls) = do
    implies' <- traverse (overConstraintArgs (traverse (goType'' ss))) implies
    return $ TypeClassDeclaration sa name args implies' deps decls
  goDecl (TypeInstanceDeclaration sa@(ss, _) ch idx name cs className tys impls) = do
    cs' <- traverse (overConstraintArgs (traverse (goType'' ss))) cs
    tys' <- traverse (goType'' ss) tys
    return $ TypeInstanceDeclaration sa ch idx name cs' className tys' impls
  goDecl (TypeSynonymDeclaration sa@(ss, _) name args ty) =
    TypeSynonymDeclaration sa name args <$> goType'' ss ty
  goDecl (TypeDeclaration (TypeDeclarationData sa@(ss, _) expr ty)) =
    TypeDeclaration . TypeDeclarationData sa expr <$> goType'' ss ty
  goDecl other =
    return other

  goExpr :: Maybe SourceSpan -> Expr -> m (Maybe SourceSpan, Expr)
  goExpr _ e@(PositionedValue pos _ _) = return (Just pos, e)
  goExpr pos (TypeClassDictionary (Constraint name tys info) dicts hints) = do
    tys' <- traverse (goType' pos) tys
    return (pos, TypeClassDictionary (Constraint name tys' info) dicts hints)
  goExpr pos (DeferredDictionary cls tys) = do
    tys' <- traverse (goType' pos) tys
    return (pos, DeferredDictionary cls tys')
  goExpr pos (TypedValue check v ty) = do
    ty' <- goType' pos ty
    return (pos, TypedValue check v ty')
  goExpr pos other = return (pos, other)

  goBinder :: Maybe SourceSpan -> Binder -> m (Maybe SourceSpan, Binder)
  goBinder _ e@(PositionedBinder pos _ _) = return (Just pos, e)
  goBinder pos (TypedBinder ty b) = do
    ty' <- goType' pos ty
    return (pos, TypedBinder ty' b)
  goBinder pos other = return (pos, other)

-- |
-- Checks all the fixity exports within a module to ensure that members aliased
-- by the operators are also exported from the module.
--
-- This pass requires name desugaring and export elaboration to have run first.
--
checkFixityExports
  :: forall m
   . MonadError MultipleErrors m
  => Module
  -> m Module
checkFixityExports (Module _ _ _ _ Nothing) =
  internalError "exports should have been elaborated before checkFixityExports"
checkFixityExports m@(Module ss _ mn ds (Just exps)) =
  rethrow (addHint (ErrorInModule mn))
    $ rethrowWithPosition ss (traverse_ checkRef exps)
    *> return m
  where

  checkRef :: DeclarationRef -> m ()
  checkRef dr@(ValueOpRef ss' op) =
    for_ (getValueOpAlias op) $ \case
      Left ident ->
        unless (ValueRef ss' ident `elem` exps)
          . throwError . errorMessage' ss'
          $ TransitiveExportError dr [ValueRef ss' ident]
      Right ctor ->
        unless (anyTypeRef (maybe False (elem ctor) . snd))
          . throwError . errorMessage' ss
          $ TransitiveDctorExportError dr ctor
  checkRef dr@(TypeOpRef ss' op) =
    for_ (getTypeOpAlias op) $ \ty ->
      unless (anyTypeRef ((== ty) . fst))
        . throwError . errorMessage' ss'
        $ TransitiveExportError dr [TypeRef ss' ty Nothing]
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

usingPredicate
  :: forall f a
   . Applicative f
  => (a -> Bool)
  -> (a -> f a)
  -> (a -> f a)
usingPredicate p f x =
  if p x then f x else pure x
