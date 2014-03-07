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

import Data.Data
import Data.Graph
import Data.Generics
import Data.List (nub, intersect)
import Control.Applicative ((<$>), (<*>), pure)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types

-- |
-- Replace all sets of mutually-recursive declarations in a module with binding groups
--
createBindingGroupsModule :: [Module] -> Either String [Module]
createBindingGroupsModule = mapM $ \(Module name ds exps) -> Module name <$> createBindingGroups name ds <*> pure exps

-- |
-- Collapse all binding groups in a module to individual declarations
--
collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule = map $ \(Module name ds exps) -> Module name (collapseBindingGroups ds) exps

-- |
-- Replace all sets of mutually-recursive declarations with binding groups
--
createBindingGroups :: ModuleName -> [Declaration] -> Either String [Declaration]
createBindingGroups moduleName ds = do
  let values = filter isValueDecl ds
      dataDecls = filter isDataDecl ds
      allProperNames = map getProperName dataDecls
      dataVerts = map (\d -> (d, getProperName d, usedProperNames moduleName d `intersect` allProperNames)) dataDecls
  dataBindingGroupDecls <- mapM toDataBindingGroup $ stronglyConnComp dataVerts
  let allIdents = map getIdent values
      valueVerts = map (\d -> (d, getIdent d, usedIdents moduleName d `intersect` allIdents)) values
      bindingGroupDecls = map toBindingGroup $ stronglyConnComp valueVerts
  return $ filter isImportDecl ds ++
           filter isExternDataDecl ds ++
           dataBindingGroupDecls ++
           filter isTypeClassDeclaration ds ++
           filter isFixityDecl ds ++
           filter isExternDecl ds ++
           bindingGroupDecls

-- |
-- Collapse all binding groups to individual declarations
--
collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups = concatMap go
  where
  go (DataBindingGroupDeclaration ds) = ds
  go (BindingGroupDeclaration ds) = map (\(ident, nameKind, val) -> ValueDeclaration ident nameKind [] Nothing val) ds
  go other = [other]

usedIdents :: (Data d) => ModuleName -> d -> [Ident]
usedIdents moduleName = nub . everything (++) (mkQ [] names)
  where
  names :: Value -> [Ident]
  names (Var (Qualified Nothing name)) = [name]
  names (Var (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  names _ = []

usedProperNames :: (Data d) => ModuleName -> d -> [ProperName]
usedProperNames moduleName = nub . everything (++) (mkQ [] names)
  where
  names :: Type -> [ProperName]
  names (TypeConstructor (Qualified (Just moduleName') name)) | moduleName == moduleName' = [name]
  names _ = []

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _ _) = ident
getIdent _ = error "Expected ValueDeclaration"

getProperName :: Declaration -> ProperName
getProperName (DataDeclaration pn _ _) = pn
getProperName (TypeSynonymDeclaration pn _ _) = pn
getProperName _ = error "Expected DataDeclaration"

toBindingGroup :: SCC Declaration -> Declaration
toBindingGroup (AcyclicSCC d) = d
toBindingGroup (CyclicSCC [d]) = d
toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration $ map fromValueDecl ds'

toDataBindingGroup :: SCC Declaration -> Either String Declaration
toDataBindingGroup (AcyclicSCC d) = return d
toDataBindingGroup (CyclicSCC [TypeSynonymDeclaration pn _ _]) = Left $ "Cycle in type synonym " ++ show pn
toDataBindingGroup (CyclicSCC [d]) = return d
toDataBindingGroup (CyclicSCC ds')
  | all isTypeSynonym ds' = Left "Cycle in type synonyms"
  | otherwise = return $ DataBindingGroupDeclaration ds'
  where
  isTypeSynonym TypeSynonymDeclaration{} = True
  isTypeSynonym _ = False

fromValueDecl :: Declaration -> (Ident, NameKind, Value)
fromValueDecl (ValueDeclaration ident nameKind [] Nothing val) = (ident, nameKind, val)
fromValueDecl ValueDeclaration{} = error "Binders should have been desugared"
fromValueDecl _ = error "Expected ValueDeclaration"
