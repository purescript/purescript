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

import Control.Monad ((<=<))
import Control.Monad.Error.Class (MonadError(..))

import Data.Graph
import Data.List (nub, intersect)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Environment
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Types

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule
  :: (MonadError MultipleErrors m)
  => ([Declaration a b] -> b)
  -> Module a b
  -> m (Module a b)
createBindingGroupsModule mkAnn (Module ss coms name ds exps) =
  Module ss coms name <$> createBindingGroups mkAnn name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module a b] -> [Module a b]
collapseBindingGroupsModule =
  map $ \(Module ss coms name ds exps) ->
    Module ss coms name (collapseBindingGroups ds) exps

createBindingGroups
  :: forall m a b
   . (MonadError MultipleErrors m)
  => ([Declaration a b] -> b)
  -> ModuleName
  -> [Declaration a b]
  -> m [Declaration a b]
createBindingGroups mkAnn moduleName = mapM f <=< handleDecls

  where
  (f, _, _) = everywhereOnValuesTopDownM return handleExprs return

  handleExprs :: Expr a b -> m (Expr a b)
  handleExprs (Let ds val ann) = flip Let val <$> handleDecls ds <*> pure ann
  handleExprs other = return other

  -- |
  -- Replace all sets of mutually-recursive declarations with binding groups
  --
  handleDecls :: [Declaration a b] -> m [Declaration a b]
  handleDecls ds = do
    let values = filter isValueDecl ds
        dataDecls = filter isDataDecl ds
        allProperNames = map declTypeName dataDecls
        dataVerts = map (\d -> (d, declTypeName d, usedTypeNames moduleName d `intersect` allProperNames)) dataDecls
    dataBindingGroupDecls <- parU (stronglyConnComp dataVerts) (toDataBindingGroup mkAnn)
    let allIdents = map declIdent values
        valueVerts = map (\d -> (d, declIdent d, usedIdents moduleName d `intersect` allIdents)) values
    bindingGroupDecls <- parU (stronglyConnComp valueVerts) (toBindingGroup mkAnn moduleName)
    return $ filter isImportDecl ds ++
             filter isExternKindDecl ds ++
             filter isExternDataDecl ds ++
             dataBindingGroupDecls ++
             filter isTypeClassDeclaration ds ++
             filter isTypeClassInstanceDeclaration ds ++
             filter isFixityDecl ds ++
             filter isExternDecl ds ++
             bindingGroupDecls

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration a b] -> [Declaration a b]
collapseBindingGroups =
  let (f, _, _) = everywhereOnValues id collapseBindingGroupsForValue id
  in map f . concatMap go
  where
  go (DataBindingGroupDeclaration ds _) = ds
  go (BindingGroupDeclaration ds ann) =
    map (\(ident, nameKind, val) ->
      ValueDeclaration ident nameKind [] [MkUnguarded val] ann) ds
  go (PositionedDeclaration pos com d ann) =
    map (flip (PositionedDeclaration pos com) ann) $ go d
  go other = [other]

collapseBindingGroupsForValue :: Expr a b -> Expr a b
collapseBindingGroupsForValue (Let ds val ann) = Let (collapseBindingGroups ds) val ann
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> Declaration a b -> [Ident]
usedIdents moduleName = nub . usedIdents' S.empty . getValue
  where
  def _ _ = []

  getValue (ValueDeclaration _ _ [] [MkUnguarded val] _) = val
  getValue ValueDeclaration{} = internalError "Binders should have been desugared"
  getValue (PositionedDeclaration _ _ d _) = getValue d
  getValue _ = internalError "Expected ValueDeclaration"

  (_, usedIdents', _, _, _) = everythingWithScope def usedNamesE def def def

  usedNamesE :: S.Set Ident -> Expr a b -> [Ident]
  usedNamesE scope (Var (Qualified Nothing name) _)
    | name `S.notMember` scope = [name]
  usedNamesE scope (Var (Qualified (Just moduleName') name) _)
    | moduleName == moduleName' && name `S.notMember` scope = [name]
  usedNamesE _ _ = []

usedImmediateIdents :: ModuleName -> Declaration a b -> [Ident]
usedImmediateIdents moduleName =
  let (f, _, _, _, _) = everythingWithContextOnValues True [] (++) def usedNamesE def def def
  in nub . f
  where
  def s _ = (s, [])

  usedNamesE :: Bool -> Expr a b -> (Bool, [Ident])
  usedNamesE True (Var (Qualified Nothing name) _) = (True, [name])
  usedNamesE True (Var (Qualified (Just moduleName') name) _)
    | moduleName == moduleName' = (True, [name])
  usedNamesE True Abs{} = (False, [])
  usedNamesE scope _ = (scope, [])

usedTypeNames :: ModuleName -> Declaration a b -> [ProperName 'TypeName]
usedTypeNames moduleName =
  let (f, _, _, _, _) = accumTypes (everythingOnTypes (++) usedNames)
  in nub . f
  where
  usedNames :: Type a -> [ProperName 'TypeName]
  usedNames (ConstrainedType constraints _ _) =
    flip mapMaybe constraints $ \case
      (Constraint (Qualified (Just moduleName') name) _ _)
        | moduleName == moduleName' -> Just (coerceProperName name)
      _ -> Nothing
  usedNames (TypeConstructor (Qualified (Just moduleName') name) _)
    | moduleName == moduleName' = [name]
  usedNames _ = []

declIdent :: Declaration a b -> Ident
declIdent (ValueDeclaration ident _ _ _ _) = ident
declIdent (PositionedDeclaration _ _ d _) = declIdent d
declIdent _ = internalError "Expected ValueDeclaration"

declTypeName :: Declaration a b -> ProperName 'TypeName
declTypeName (DataDeclaration _ pn _ _ _) = pn
declTypeName (TypeSynonymDeclaration pn _ _ _) = pn
declTypeName (PositionedDeclaration _ _ d _) = declTypeName d
declTypeName _ = internalError "Expected DataDeclaration"

-- |
-- Convert a group of mutually-recursive dependencies into a BindingGroupDeclaration (or simple ValueDeclaration).
--
--
toBindingGroup
  :: forall m a b
   . (MonadError MultipleErrors m)
   => ([Declaration a b] -> b)
   -> ModuleName
   -> SCC (Declaration a b)
   -> m (Declaration a b)
toBindingGroup _ _ (AcyclicSCC d) = return d
toBindingGroup mkAnn moduleName (CyclicSCC ds') =
  -- Once we have a mutually-recursive group of declarations, we need to sort
  -- them further by their immediate dependencies (those outside function
  -- bodies). In particular, this is relevant for type instance dictionaries
  -- whose members require other type instances (for example, functorEff
  -- defines (<$>) = liftA1, which depends on applicativeEff). Note that
  -- superclass references are still inside functions, so don't count here.
  -- If we discover declarations that still contain mutually-recursive
  -- immediate references, we're guaranteed to get an undefined reference at
  -- runtime, so treat this as an error. See also github issue #365.
  BindingGroupDeclaration <$> mapM toBinding (stronglyConnComp valueVerts) <*> pure (mkAnn ds')
  where
  idents :: [Ident]
  idents = map (\(_, i, _) -> i) valueVerts

  valueVerts :: [(Declaration a b, Ident, [Ident])]
  valueVerts = map (\d -> (d, declIdent d, usedImmediateIdents moduleName d `intersect` idents)) ds'

  toBinding :: SCC (Declaration a b) -> m (Ident, NameKind, Expr a b)
  toBinding (AcyclicSCC d) = return $ fromValueDecl d
  toBinding (CyclicSCC ds) = throwError $ foldMap cycleError ds

  cycleError :: Declaration a b -> MultipleErrors
  cycleError (PositionedDeclaration p _ d _) = onErrorMessages (withPosition p) $ cycleError d
  cycleError (ValueDeclaration n _ _ [MkUnguarded _] _) = errorMessage $ CycleInDeclaration n
  cycleError _ = internalError "cycleError: Expected ValueDeclaration"

toDataBindingGroup
  :: MonadError MultipleErrors m
  => ([Declaration a b] -> b)
  -> SCC (Declaration a b)
  -> m (Declaration a b)
toDataBindingGroup _ (AcyclicSCC d) = return d
toDataBindingGroup _ (CyclicSCC [d]) = case isTypeSynonym d of
  Just pn -> throwError . errorMessage $ CycleInTypeSynonym (Just pn)
  _ -> return d
toDataBindingGroup mkAnn (CyclicSCC ds')
  | all (isJust . isTypeSynonym) ds' = throwError . errorMessage $ CycleInTypeSynonym Nothing
  | otherwise = return $ DataBindingGroupDeclaration ds' (mkAnn ds')

isTypeSynonym :: Declaration a b -> Maybe (ProperName 'TypeName)
isTypeSynonym (TypeSynonymDeclaration pn _ _ _) = Just pn
isTypeSynonym (PositionedDeclaration _ _ d _) = isTypeSynonym d
isTypeSynonym _ = Nothing

fromValueDecl :: Declaration a b -> (Ident, NameKind, Expr a b)
fromValueDecl (ValueDeclaration ident nameKind [] [MkUnguarded val] _) = (ident, nameKind, val)
fromValueDecl ValueDeclaration{} = internalError "Binders should have been desugared"
fromValueDecl (PositionedDeclaration _ _ d _) = fromValueDecl d
fromValueDecl _ = internalError "Expected ValueDeclaration"
