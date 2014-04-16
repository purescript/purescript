-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.BindingGroups
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module implements the desugaring pass which creates binding groups from sets of
-- mutually-recursive value declarations and mutually-recursive type declarations.
--
-----------------------------------------------------------------------------

module Language.PureScript.Sugar.BindingGroups (
    createBindingGroups,
    createBindingGroupsModule,
    collapseBindingGroups,
    collapseBindingGroupsModule
) where

import Data.Graph
import Data.List (nub, intersect)
import Data.Maybe (isJust, mapMaybe)
import Control.Applicative ((<$>), (<*>), pure)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Types
import Language.PureScript.Environment
import Language.PureScript.Errors

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule :: [Module] -> Either ErrorStack [Module]
createBindingGroupsModule = mapM $ \(Module name ds exps) -> Module name <$> createBindingGroups name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule = map $ \(Module name ds exps) -> Module name (collapseBindingGroups ds) exps

-- |
-- Replace all sets of mutually-recursive declarations with binding groups
--
createBindingGroups :: ModuleName -> [Declaration] -> Either ErrorStack [Declaration]
createBindingGroups moduleName ds = do
  values <- mapM (createBindingGroupsForValue moduleName) $ filter isValueDecl ds
  let dataDecls = filter isDataDecl ds
      allProperNames = map getProperName dataDecls
      dataVerts = map (\d -> (d, getProperName d, usedProperNames moduleName d `intersect` allProperNames)) dataDecls
  dataBindingGroupDecls <- mapM toDataBindingGroup $ stronglyConnComp dataVerts
  let allIdents = map getIdent values
      valueVerts = map (\d -> (d, getIdent d, usedIdents moduleName d `intersect` allIdents)) values
      bindingGroupDecls = map toBindingGroup $ stronglyConnComp valueVerts
  return $ filter isImportDecl ds ++
           filter isExternDataDecl ds ++
           filter isExternInstanceDecl ds ++
           dataBindingGroupDecls ++
           filter isTypeClassDeclaration ds ++
           filter isFixityDecl ds ++
           filter isExternDecl ds ++
           bindingGroupDecls

createBindingGroupsForValue :: ModuleName -> Declaration -> Either ErrorStack Declaration
createBindingGroupsForValue moduleName =
  let (f, _, _) = everywhereOnValuesTopDownM return go return
  in f
  where
  go (Let ds val) = Let <$> createBindingGroups moduleName ds <*> pure val
  go other = return other

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups = let (f, _, _) = everywhereOnValues id collapseBindingGroupsForValue id in map f . concatMap go
  where
  go (DataBindingGroupDeclaration ds) = ds
  go (BindingGroupDeclaration ds) = map (\(ident, nameKind, val) -> ValueDeclaration ident nameKind [] Nothing val) ds
  go (PositionedDeclaration pos d) = map (PositionedDeclaration pos) $ go d
  go other = [other]

collapseBindingGroupsForValue :: Value -> Value
collapseBindingGroupsForValue (Let ds val) = Let (collapseBindingGroups ds) val
collapseBindingGroupsForValue other = other

usedIdents :: ModuleName -> Declaration -> [Ident]
usedIdents moduleName =
  let (f, _, _, _, _) = everythingOnValues (++) (const []) usedNames (const []) (const []) (const [])
  in nub . f
  where
  usedNames :: Value -> [Ident]
  usedNames (Var (Qualified Nothing name)) = [name]
  usedNames (Var (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  usedNames _ = []

usedProperNames :: ModuleName -> Declaration -> [ProperName]
usedProperNames moduleName =
  let (f, _, _, _, _) = accumTypes (everythingOnTypes (++) usedNames)
  in nub . f
  where
  usedNames :: Type -> [ProperName]
  usedNames (ConstrainedType constraints _) = flip mapMaybe constraints $ \qual ->
    case qual of
      (Qualified (Just moduleName') name, _) | moduleName == moduleName' -> Just name
      _ -> Nothing
  usedNames (TypeConstructor (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  usedNames _ = []

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _ _) = ident
getIdent (PositionedDeclaration _ d) = getIdent d
getIdent _ = error "Expected ValueDeclaration"

getProperName :: Declaration -> ProperName
getProperName (DataDeclaration pn _ _) = pn
getProperName (TypeSynonymDeclaration pn _ _) = pn
getProperName (PositionedDeclaration _ d) = getProperName d
getProperName _ = error "Expected DataDeclaration"

toBindingGroup :: SCC Declaration -> Declaration
toBindingGroup (AcyclicSCC d) = d
toBindingGroup (CyclicSCC [d]) = d
toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration $ map fromValueDecl ds'

toDataBindingGroup :: SCC Declaration -> Either ErrorStack Declaration
toDataBindingGroup (AcyclicSCC d) = return d
toDataBindingGroup (CyclicSCC [d]) = case isTypeSynonym d of
  Just pn -> Left $ mkErrorStack ("Cycle in type synonym " ++ show pn) Nothing
  _ -> return d
toDataBindingGroup (CyclicSCC ds')
  | all (isJust . isTypeSynonym) ds' = Left $ mkErrorStack "Cycle in type synonyms" Nothing
  | otherwise = return $ DataBindingGroupDeclaration ds'

isTypeSynonym :: Declaration -> Maybe ProperName
isTypeSynonym (TypeSynonymDeclaration pn _ _) = Just pn
isTypeSynonym (PositionedDeclaration _ d) = isTypeSynonym d
isTypeSynonym _ = Nothing

fromValueDecl :: Declaration -> (Ident, NameKind, Value)
fromValueDecl (ValueDeclaration ident nameKind [] Nothing val) = (ident, nameKind, val)
fromValueDecl ValueDeclaration{} = error "Binders should have been desugared"
fromValueDecl (PositionedDeclaration _ d) = fromValueDecl d
fromValueDecl _ = error "Expected ValueDeclaration"
