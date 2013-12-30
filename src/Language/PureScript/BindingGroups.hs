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

import Data.Graph
import Data.List (intersect)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Values
import Language.PureScript.Scope (usedNames)

createBindingGroupsModule :: [Module] -> [Module]
createBindingGroupsModule = map $ \(Module name ds) -> Module name (createBindingGroups ds)

createBindingGroups :: [Declaration] -> [Declaration]
createBindingGroups ds =
  let
    values = filter isValueDecl ds
    nonValues = filter (not . isValueDecl) ds
    allIdents = map getIdent values
    verts = map (\d -> (d, getIdent d, usedNames d `intersect` allIdents)) values
    sorted = map toBindingGroup $ stronglyConnComp verts
  in
    nonValues ++ sorted

isValueDecl :: Declaration -> Bool
isValueDecl (ValueDeclaration _ _ _ _) = True
isValueDecl _ = False

getIdent :: Declaration -> Ident
getIdent (ValueDeclaration ident _ _ _) = ident
getIdent _ = error "Expected ValueDeclaration"

toBindingGroup :: SCC Declaration -> Declaration
toBindingGroup (AcyclicSCC d) = d
toBindingGroup (CyclicSCC [d]) = d
toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration (map fromValueDecl ds')

fromValueDecl :: Declaration -> (Ident, Value)
fromValueDecl (ValueDeclaration ident [] Nothing val) = (ident, val)
fromValueDecl (ValueDeclaration _ _ _ _) = error "Binders should have been desugared"
fromValueDecl _ = error "Expected ValueDeclaration"
