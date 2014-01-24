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
import Control.Applicative ((<$>))

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types
import Debug.Trace (trace)

createBindingGroupsModule :: [Module] -> Either String [Module]
createBindingGroupsModule = mapM $ \(Module name ds) -> Module name <$> createBindingGroups ds

collapseBindingGroupsModule :: [Module] -> [Module]
collapseBindingGroupsModule = map $ \(Module name ds) -> Module name (collapseBindingGroups ds)

createBindingGroups :: [Declaration] -> Either String [Declaration]
createBindingGroups ds = do
  let values = filter isValueDecl ds
      dataDecls = filter isDataDecl ds
      allProperNames = map getProperName dataDecls
      dataVerts = map (\d -> (d, getProperName d, usedProperNames d `intersect` allProperNames)) dataDecls
  dataBindingGroupDecls <- mapM toDataBindingGroup $ stronglyConnComp dataVerts
  let allIdents = map getIdent values
      valueVerts = map (\d -> (d, getIdent d, usedIdents d `intersect` allIdents)) values
      bindingGroupDecls = map toBindingGroup $ stronglyConnComp valueVerts
  return $ filter isImportDecl ds ++
           filter isExternDataDecl ds ++
           dataBindingGroupDecls ++
           filter isTypeClassDeclaration ds ++
           filter isFixityDecl ds ++
           filter isExternDecl ds ++
           bindingGroupDecls

collapseBindingGroups :: [Declaration] -> [Declaration]
collapseBindingGroups ds = concatMap go ds
  where
  go (DataBindingGroupDeclaration ds) = ds
  go (BindingGroupDeclaration ds) = map (\(ident, val) -> ValueDeclaration ident [] Nothing val) ds
  go other = [other]

usedIdents :: (Data d) => d -> [Ident]
usedIdents = nub . everything (++) (mkQ [] names)
  where
  names :: Value -> [Ident]
  names (Var (Qualified Nothing name)) = [name]
  names _ = []

usedProperNames :: (Data d) => d -> [ProperName]
usedProperNames = nub . everything (++) (mkQ [] names)
  where
  names :: Type -> [ProperName]
  names (TypeConstructor (Qualified Nothing name)) = [name]
  names _ = []

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _) = ident
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
  isTypeSynonym (TypeSynonymDeclaration _ _ _) = True
  isTypeSynonym _ = False

fromValueDecl :: Declaration -> (Ident, Value)
fromValueDecl (ValueDeclaration ident [] Nothing val) = (ident, val)
fromValueDecl (ValueDeclaration _ _ _ _) = error "Binders should have been desugared"
fromValueDecl _ = error "Expected ValueDeclaration"
