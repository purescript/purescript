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

module Language.PureScript.BindingGroups (
    createBindingGroups,
    createBindingGroupsModule
) where

import Data.Data
import Data.Graph
import Data.Generics
import Data.List (nub, intersect)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Types

createBindingGroupsModule :: [Module] -> [Module]
createBindingGroupsModule = map $ \(Module name ds) -> Module name (createBindingGroups ds)

createBindingGroups :: [Declaration] -> [Declaration]
createBindingGroups ds =
  let
    values = filter isValueDecl ds
    dataDecls = filter isDataDecl ds
    nonValues = filter (\d -> not (isValueDecl d) && not (isDataDecl d)) ds
    allProperNames = map getProperName dataDecls
    dataVerts = map (\d -> (d, getProperName d, usedProperNames d `intersect` allProperNames)) dataDecls
    dataBindingGroupDecls = map toDataBindingGroup $ stronglyConnComp dataVerts
    allIdents = map getIdent values
    valueVerts = map (\d -> (d, getIdent d, usedIdents d `intersect` allIdents)) values
    bindingGroupDecls = map toBindingGroup $ stronglyConnComp valueVerts
  in
    dataBindingGroupDecls ++ nonValues ++ bindingGroupDecls

usedIdents :: (Data d) => d -> [Ident]
usedIdents = nub . everything (++) (mkQ [] namesV `extQ` namesS)
  where
  namesV :: Value -> [Ident]
  namesV (Var (Qualified Nothing name)) = [name]
  namesV _ = []
  namesS :: Statement -> [Ident]
  namesS (VariableIntroduction name _) = [name]
  namesS _ = []

usedProperNames :: (Data d) => d -> [ProperName]
usedProperNames = nub . everything (++) (mkQ [] names)
  where
  names :: Type -> [ProperName]
  names (TypeConstructor (Qualified Nothing name)) = [name]
  names _ = []

isValueDecl :: Declaration -> Bool
isValueDecl (ValueDeclaration _ _ _ _) = True
isValueDecl _ = False

isDataDecl :: Declaration -> Bool
isDataDecl (DataDeclaration _ _ _) = True
isDataDecl _ = False

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _) = ident
getIdent _ = error "Expected ValueDeclaration"

getProperName :: Declaration -> ProperName
getProperName (DataDeclaration pn _ _) = pn
getProperName _ = error "Expected DataDeclaration"

toBindingGroup :: SCC Declaration -> Declaration
toBindingGroup (AcyclicSCC d) = d
toBindingGroup (CyclicSCC [d]) = d
toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration $ map fromValueDecl ds'

toDataBindingGroup :: SCC Declaration -> Declaration
toDataBindingGroup (AcyclicSCC d) = d
toDataBindingGroup (CyclicSCC [d]) = d
toDataBindingGroup (CyclicSCC ds') = DataBindingGroupDeclaration $ map fromDataDecl ds'

fromValueDecl :: Declaration -> (Ident, Value)
fromValueDecl (ValueDeclaration ident [] Nothing val) = (ident, val)
fromValueDecl (ValueDeclaration _ _ _ _) = error "Binders should have been desugared"
fromValueDecl _ = error "Expected ValueDeclaration"

fromDataDecl :: Declaration -> (ProperName, [String], [(ProperName, Maybe PolyType)])
fromDataDecl (DataDeclaration pn args ctors) = (pn, args, ctors)
fromDataDecl _ = error "Expected DataDeclaration"
