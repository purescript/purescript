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
    createBindingGroups
) where

import Data.Graph
import Data.List (intersect)

import Language.PureScript.Declarations
import Language.PureScript.Names
import Language.PureScript.Scope (usedNames)

createBindingGroups :: [Declaration] -> [Declaration]
createBindingGroups ds =
  let
    values = filter isValueDecl ds
    nonValues = filter (not . isValueDecl) ds
    allIdents = map getIdent values
    verts = map (\d -> (d, getIdent d, usedNames d `intersect` allIdents)) values
    sorted = map toBindingGroup $ stronglyConnComp verts
  in
    map handleModuleDeclaration nonValues ++ sorted
  where
  isValueDecl :: Declaration -> Bool
  isValueDecl (ValueDeclaration _ _ _ _) = True
  isValueDecl _ = False
  getIdent :: Declaration -> Ident
  getIdent (ValueDeclaration ident _ _ _) = ident
  getIdent _ = error "undefined"
  toBindingGroup :: SCC Declaration -> Declaration
  toBindingGroup (AcyclicSCC d) = d
  toBindingGroup (CyclicSCC [d]) = d
  toBindingGroup (CyclicSCC ds') = BindingGroupDeclaration ds'
  handleModuleDeclaration :: Declaration -> Declaration
  handleModuleDeclaration (ModuleDeclaration name ds') = ModuleDeclaration name $ createBindingGroups ds'
  handleModuleDeclaration other = other
