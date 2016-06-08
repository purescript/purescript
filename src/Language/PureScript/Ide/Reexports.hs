-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Reexports
-- Description : Resolves reexports for psc-ide
-- Copyright   : Christoph Hegemann 2016
--               Brian Sermons 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Resolves reexports for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.Reexports where


import           Prelude                       ()
import           Prelude.Compat

import           Data.List                     (union)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe
import           Language.PureScript.Ide.Types

getReexports :: Module -> [ExternDecl]
getReexports (mn, decls)= concatMap getExport decls
  where getExport d
          | (Export mn') <- d
          , mn /= mn' = replaceExportWithAliases decls mn'
          | otherwise = []

dependencyToExport :: ExternDecl -> ExternDecl
dependencyToExport (Dependency m _ _) = Export m
dependencyToExport decl = decl

replaceExportWithAliases :: [ExternDecl] -> ModuleIdent -> [ExternDecl]
replaceExportWithAliases decls ident =
  case filter isMatch decls of
    [] -> [Export ident]
    aliases -> map dependencyToExport aliases
  where isMatch d
          | Dependency _ _ (Just alias) <- d
          , alias == ident = True
          | otherwise = False

replaceReexport :: ExternDecl -> Module -> Module -> Module
replaceReexport e@(Export _) (m, decls) (_, newDecls) =
  (m, filter (/= e) decls `union` newDecls)
replaceReexport _ _ _ = error "Should only get Exports here."

emptyModule :: Module
emptyModule = ("Empty", [])

isExport :: ExternDecl -> Bool
isExport (Export _) = True
isExport _ = False

removeExportDecls :: Module -> Module
removeExportDecls = fmap (filter (not . isExport))

replaceReexports :: Module -> Map ModuleIdent [ExternDecl] -> Module
replaceReexports m db = result
  where
    reexports = getReexports m
    result = foldl go (removeExportDecls m) reexports

    go :: Module -> ExternDecl -> Module
    go m' re@(Export name) = replaceReexport re m' (getModule name)
    go _ _ = error "partiality! woohoo"

    getModule :: ModuleIdent -> Module
    getModule name = clean res
      where
        res = fromMaybe emptyModule $ (name , ) <$> Map.lookup name db
        -- we have to do this because keeping self exports in will result in
        -- infinite loops
        clean (mn, decls) = (mn,) (filter (/= Export mn) decls)

resolveReexports :: Map ModuleIdent [ExternDecl] -> Module ->  Module
resolveReexports modules m =
  let replaced = replaceReexports m modules
  in if null (getReexports replaced)
     then replaced
     else resolveReexports modules replaced
