-- |
-- This module implements the desugaring pass which creates binding groups from sets of
-- mutually-recursive value declarations and mutually-recursive type declarations.
--
module Language.PureScript.Sugar.BindingGroups
  ( createBindingGroups
  , createBindingGroupsModule
  , collapseBindingGroups
  , collapseBindingGroupsModule
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Monad (void, (<=<))
import Control.Monad.Error.Class (MonadError(..))

import Data.Graph
import Data.List (intersect)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.PSString (PSString, prettyPrintString)
import Language.PureScript.Types

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule
  :: (MonadError MultipleErrors m)
  => Module
  -> m Module
createBindingGroupsModule (Module ss coms name ds exps) =
  Module ss coms name <$> createBindingGroups name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule =
  fmap $ \(Module ss coms name ds exps) ->
    Module ss coms name (collapseBindingGroups ds) exps

createBindingGroups
  :: forall m
   . (MonadError MultipleErrors m)
  => ModuleName
  -> [Declaration]
  -> m [Declaration]
createBindingGroups moduleName = mapM f <=< handleDecls
  where
  (f, _, _) = everywhereOnValuesTopDownM return handleExprs return

  handleExprs :: Expr -> m Expr
  handleExprs (Let w ds val) = (\ds' -> Let w ds' val) <$> handleDecls ds
  handleExprs other = return other

  -- |
  -- Replace all sets of mutually-recursive declarations with binding groups
  --
  handleDecls :: [Declaration] -> m [Declaration]
  handleDecls ds = do
    let dataDecls = filter isDataDecl ds
        allProperNames = fmap declTypeName dataDecls
        dataVerts = fmap (\d -> (d, declTypeName d, usedTypeNames moduleName d `intersect` allProperNames)) dataDecls
    dataBindingGroupDecls <- parU (stronglyConnComp dataVerts) toDataBindingGroup

    let values = mapMaybe (fmap (fmap extractGuardedExpr) . getValueDeclaration) ds
        allIdents = fmap valdeclIdent values
        valueVerts = fmap (\d -> (d, valdeclIdent d, usedIdents moduleName d `intersect` allIdents)) values
    bindingGroupDecls <- parU (stronglyConnComp valueVerts) (toBindingGroups usedImmediateIdents moduleName)

    let dictValues = mapMaybe (fmap (fmap extractGuardedExpr) . getDictDeclaration) ds
        dictIdents = fmap valdeclIdent dictValues
        dictValueVerts = fmap (\d -> (d, valdeclIdent d, usedIdents moduleName d `intersect` dictIdents)) dictValues
        checkDeclsForInvalidCycles = toBindingGroups
    void $ parU (stronglyConnComp dictValueVerts) (checkDeclsForInvalidCycles immediateLitIdentsAndAllOtherIdents moduleName)

    return $ filter isImportDecl ds ++
             filter isExternKindDecl ds ++
             filter isExternDataDecl ds ++
             dataBindingGroupDecls ++
             filter isTypeClassDeclaration ds ++
             filter isTypeClassInstanceDeclaration ds ++
             filter isFixityDecl ds ++
             filter isExternDecl ds ++
             bindingGroupDecls
    where
      extractGuardedExpr [MkUnguarded expr] = expr
      extractGuardedExpr _ = internalError "Expected Guards to have been desugared in handleDecls."

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups =
  let (f, _, _) = everywhereOnValues id collapseBindingGroupsForValue id
  in fmap f . concatMap go
  where
  go (DataBindingGroupDeclaration ds) = NEL.toList ds
  go (BindingGroupDeclaration ds) =
    NEL.toList $ fmap (\((sa, ident), nameKind, val) ->
      ValueDecl sa ident nameKind [] [MkUnguarded val]) ds
  go other = [other]

collapseBindingGroupsForValue :: Expr -> Expr
collapseBindingGroupsForValue (Let w ds val) = Let w (collapseBindingGroups ds) val
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> ValueDeclarationData Expr -> [Ident]
usedIdents moduleName = ordNub . usedIdents' S.empty . valdeclExpression
  where
  def _ _ = []

  (_, usedIdents', _, _, _) = everythingWithScope def usedNamesE def def def

  usedNamesE :: S.Set ScopedIdent -> Expr -> [Ident]
  usedNamesE scope (Var _ (Qualified Nothing name))
    | LocalIdent name `S.notMember` scope = [name]
  usedNamesE scope (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' && ToplevelIdent name `S.notMember` scope = [name]
  usedNamesE _ _ = []

immediateLitIdentsAndAllOtherIdents :: ModuleName -> Expr -> [Ident]
immediateLitIdentsAndAllOtherIdents moduleName = ordNub . g
  where
  (_, g, _, _, _) = everythingWithContextOnValues True [] (++) def usedNamesE usedNamesLitE def def def

  def s _ = (s, [])

  usedNamesE :: Bool -> Expr -> (Bool, [Ident])
  usedNamesE True (Var _ (Qualified Nothing name)) = (True, [name])
  usedNamesE True (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = (True, [name])
  usedNamesE scope _ = (scope, [])

  usedNamesLitE :: Bool -> Expr -> (Bool, [Ident])
  usedNamesLitE True (Var _ (Qualified Nothing name)) = (True, [name])
  usedNamesLitE True (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = (True, [name])
  usedNamesLitE True (App (stripTypedAndPositioned -> (Abs _ e0)) e1) = (True, g e0 ++ g e1)
  usedNamesLitE True (Abs _ _) = (False, [])
  usedNamesLitE scope _ = (scope, [])

usedImmediateIdents :: ModuleName -> Expr -> [Ident]
usedImmediateIdents moduleName = ordNub . g
  where
  (_, g, _, _, _) = everythingWithContextOnValues True [] (++) def usedNamesE usedNamesE def def def

  def s _ = (s, [])

  usedNamesE :: Bool -> Expr -> (Bool, [Ident])
  usedNamesE True (Var _ (Qualified Nothing name)) = (True, [name])
  usedNamesE True (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = (True, [name])
  usedNamesE True (Abs _ _) = (False, [])
  usedNamesE scope _ = (scope, [])

usedTypeNames :: ModuleName -> Declaration -> [ProperName 'TypeName]
usedTypeNames moduleName = ordNub . f
  where
  (f, _, _, _, _) = accumTypes (everythingOnTypes (++) usedNames)

  usedNames :: SourceType -> [ProperName 'TypeName]
  usedNames (ConstrainedType _ con _) =
    case con of
      (Constraint _ (Qualified (Just moduleName') name) _ _)
        | moduleName == moduleName' -> [coerceProperName name]
      _ -> []
  usedNames (TypeConstructor _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = [name]
  usedNames _ = []

declTypeName :: Declaration -> ProperName 'TypeName
declTypeName (DataDeclaration _ _ pn _ _) = pn
declTypeName (TypeSynonymDeclaration _ pn _ _) = pn
declTypeName _ = internalError "Expected DataDeclaration"

-- |
-- Convert a group of mutually-recursive dependencies into a BindingGroupDeclaration (or simple ValueDeclaration).
--
--
toBindingGroups
  :: forall m
   . (MonadError MultipleErrors m)
   => (ModuleName -> Expr -> [Ident])
   -> ModuleName
   -> SCC (ValueDeclarationData Expr)
   -> m Declaration
toBindingGroups _ _ (AcyclicSCC d) = return (mkDeclaration d)
toBindingGroups getIdents moduleName (CyclicSCC ds') = do
  -- Once we have a mutually-recursive group of declarations, we need to sort
  -- them further by appropriate subsets of their dependencies (generally, those
  -- outside function bodies). In particular, this is relevant for type instance
  -- dictionaries whose members require other type instances (for example,
  -- functorEff defines (<$>) = liftA1, which depends on applicativeEff). Note
  -- that superclass references are still inside functions, so don't count here.
  -- If we discover declarations that still contain mutually-recursive
  -- immediate references, we're guaranteed to get an undefined reference at
  -- runtime, so treat this as an error.
  -- (See also github issues #365, #2975, and #3429.)
  BindingGroupDeclaration . NEL.fromList <$> mapM toBinding (stronglyConnComp valueVerts)
  where
  cycleErrors :: ValueDeclarationData Expr -> MultipleErrors
  cycleErrors (ValueDeclarationData (ss, _) n _ _ e) =
    case (e, stripAbs e) of
      (TypedValue _ (stripTypedAndPositioned -> (Var _ _)) st, _)
        | isFunctionType st -> errorMessage' ss $ MissingEtaExpansion n
      (_, TypeClassDictionaryConstructorApp _ (getFields -> Just fields)) ->
        let getData = map getFieldData . filter (refersToIdent n . snd)
        in errorMessage' ss $ CycleInDictDeclaration n $ getData fields
      _ -> errorMessage' ss $ CycleInDeclaration n

  getFieldData :: (PSString, Expr) -> (Ident, SourceSpan, DictMemberType)
  getFieldData (psString, expr) = case separateValAndType expr of
    (expr', mSt) -> (ident, getSourceSpan expr', isFn mSt)
    where
    ident = Ident $ prettyPrintString psString
    isFn = maybe NonFn (\st -> if isFunctionType st then Fn else NonFn)

  getFields :: Expr -> Maybe [(PSString, Expr)]
  getFields expr = case stripTypedAndPositioned expr of
    Literal _ (ObjectLiteral fields) -> Just fields
    _ -> Nothing

  getSourceSpan :: Expr -> SourceSpan
  getSourceSpan (PositionedValue ss _ _) = ss
  getSourceSpan (Literal ss _) = ss
  getSourceSpan (UnaryMinus ss _) = ss
  getSourceSpan (Var ss _) = ss
  getSourceSpan (Op ss _) = ss
  getSourceSpan (Constructor ss _) = ss
  getSourceSpan (App (stripTypedAndPositioned -> (Abs _ expr)) _) = getSourceSpan expr
  getSourceSpan (TypedValue _ expr _) = getSourceSpan expr
  getSourceSpan _ = NullSourceSpan

  idents :: [Ident]
  idents = fmap (\(_, i, _) -> i) valueVerts

  refersToIdent :: Ident -> Expr -> Bool
  refersToIdent ident = elem ident . getIdents moduleName

  separateValAndType :: Expr -> (Expr, Maybe SourceType)
  separateValAndType (TypedValue _ e st) =
    let (e', _) = separateValAndType e
    in  (e', Just st)
  separateValAndType e = (e, Nothing)

  stripAbs :: Expr -> Expr
  stripAbs e = case stripTypedAndPositioned e of
    Abs _ e' -> stripAbs e'
    e' -> e'

  toBinding :: SCC (ValueDeclarationData Expr) -> m ((SourceAnn, Ident), NameKind, Expr)
  toBinding (AcyclicSCC d) = return $ fromValueDecl d
  toBinding (CyclicSCC ds) = throwError $ foldMap cycleErrors ds

  valueVerts :: [(ValueDeclarationData Expr, Ident, [Ident])]
  valueVerts = fmap (\d -> (d, valdeclIdent d, getIdents moduleName (valdeclExpression d) `intersect` idents)) ds'

toDataBindingGroup
  :: MonadError MultipleErrors m
  => SCC Declaration
  -> m Declaration
toDataBindingGroup (AcyclicSCC d) = return d
toDataBindingGroup (CyclicSCC [d]) = case isTypeSynonym d of
  Just pn -> throwError . errorMessage' (declSourceSpan d) $ CycleInTypeSynonym (Just pn)
  _ -> return d
toDataBindingGroup (CyclicSCC ds')
  | all (isJust . isTypeSynonym) ds' = throwError . errorMessage' (declSourceSpan (head ds')) $ CycleInTypeSynonym Nothing
  | otherwise = return . DataBindingGroupDeclaration $ NEL.fromList ds'

isTypeSynonym :: Declaration -> Maybe (ProperName 'TypeName)
isTypeSynonym (TypeSynonymDeclaration _ pn _ _) = Just pn
isTypeSynonym _ = Nothing

mkDeclaration :: ValueDeclarationData Expr -> Declaration
mkDeclaration = ValueDeclaration . fmap (pure . MkUnguarded)

fromValueDecl :: ValueDeclarationData Expr -> ((SourceAnn, Ident), NameKind, Expr)
fromValueDecl (ValueDeclarationData sa ident nameKind [] val) = ((sa, ident), nameKind, val)
fromValueDecl ValueDeclarationData{} = internalError "Binders should have been desugared"
