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
  handleExprs (Let ann ds val) = Let ann <$> handleDecls ds <*> pure val
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
  go (DataBindingGroupDeclaration _ ds) = ds
  go (BindingGroupDeclaration ann ds) =
    map (\(ident, nameKind, val) ->
      ValueDeclaration ann ident nameKind [] [MkUnguarded val]) ds
  go (PositionedDeclaration ann pos com d) =
    map (PositionedDeclaration ann pos com) $ go d
  go other = [other]

collapseBindingGroupsForValue :: Expr a b -> Expr a b
collapseBindingGroupsForValue (Let ann ds val) = Let ann (collapseBindingGroups ds) val
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> Declaration a b -> [Ident]
usedIdents moduleName = nub . usedIdents' S.empty . getValue
  where
  def _ _ = []

  getValue (ValueDeclaration _ _ _ [] [MkUnguarded val]) = val
  getValue ValueDeclaration{} = internalError "Binders should have been desugared"
  getValue (PositionedDeclaration _ _ _ d) = getValue d
  getValue _ = internalError "Expected ValueDeclaration"

  (_, usedIdents', _, _, _) = everythingWithScope def usedNamesE def def def

  usedNamesE :: S.Set Ident -> Expr a b -> [Ident]
  usedNamesE scope (Var _ (Qualified Nothing name))
    | name `S.notMember` scope = [name]
  usedNamesE scope (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' && name `S.notMember` scope = [name]
  usedNamesE _ _ = []

usedImmediateIdents :: ModuleName -> Declaration a b -> [Ident]
usedImmediateIdents moduleName =
  let (f, _, _, _, _) = everythingWithContextOnValues True [] (++) def usedNamesE def def def
  in nub . f
  where
  def s _ = (s, [])

  usedNamesE :: Bool -> Expr a b -> (Bool, [Ident])
  usedNamesE True (Var _ (Qualified Nothing name)) = (True, [name])
  usedNamesE True (Var _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = (True, [name])
  usedNamesE True Abs{} = (False, [])
  usedNamesE scope _ = (scope, [])

usedTypeNames :: ModuleName -> Declaration a b -> [ProperName 'TypeName]
usedTypeNames moduleName =
  let (f, _, _, _, _) = accumTypes (everythingOnTypes (++) usedNames)
  in nub . f
  where
  usedNames :: Type a -> [ProperName 'TypeName]
  usedNames (ConstrainedType _ constraints _) =
    flip mapMaybe constraints $ \case
      (Constraint (Qualified (Just moduleName') name) _ _)
        | moduleName == moduleName' -> Just (coerceProperName name)
      _ -> Nothing
  usedNames (TypeConstructor _ (Qualified (Just moduleName') name))
    | moduleName == moduleName' = [name]
  usedNames _ = []

declIdent :: Declaration a b -> Ident
declIdent (ValueDeclaration _ ident _ _ _) = ident
declIdent (PositionedDeclaration _ _ _ d) = declIdent d
declIdent _ = internalError "Expected ValueDeclaration"

declTypeName :: Declaration a b -> ProperName 'TypeName
declTypeName (DataDeclaration _ _ pn _ _) = pn
declTypeName (TypeSynonymDeclaration _ pn _ _) = pn
declTypeName (PositionedDeclaration _ _ _ d) = declTypeName d
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
  BindingGroupDeclaration (mkAnn ds') <$> mapM toBinding (stronglyConnComp valueVerts)
  where
  idents :: [Ident]
  idents = map (\(_, i, _) -> i) valueVerts

  valueVerts :: [(Declaration a b, Ident, [Ident])]
  valueVerts = map (\d -> (d, declIdent d, usedImmediateIdents moduleName d `intersect` idents)) ds'

  toBinding :: SCC (Declaration a b) -> m (Ident, NameKind, Expr a b)
  toBinding (AcyclicSCC d) = return $ fromValueDecl d
  toBinding (CyclicSCC ds) = throwError $ foldMap cycleError ds

  cycleError :: Declaration a b -> MultipleErrors
  cycleError (PositionedDeclaration _ p _ d) = onErrorMessages (withPosition p) $ cycleError d
  cycleError (ValueDeclaration _ n _ _ [MkUnguarded _]) = errorMessage $ CycleInDeclaration n
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
  | otherwise = return $ DataBindingGroupDeclaration (mkAnn ds') ds'

isTypeSynonym :: Declaration a b -> Maybe (ProperName 'TypeName)
isTypeSynonym (TypeSynonymDeclaration _ pn _ _) = Just pn
isTypeSynonym (PositionedDeclaration _ _ _ d) = isTypeSynonym d
isTypeSynonym _ = Nothing

fromValueDecl :: Declaration a b -> (Ident, NameKind, Expr a b)
fromValueDecl (ValueDeclaration _ ident nameKind [] [MkUnguarded val]) = (ident, nameKind, val)
fromValueDecl ValueDeclaration{} = internalError "Binders should have been desugared"
fromValueDecl (PositionedDeclaration _ _ _ d) = fromValueDecl d
fromValueDecl _ = internalError "Expected ValueDeclaration"
