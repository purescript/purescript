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

module Language.PureScript.Ide.Reexports
  ( resolveReexports2
  -- for tests
  , getReexports
  , replaceReexport
  , replaceReexports
  ) where


import           Protolude

import           Data.List                     (union)
import qualified Data.Map                      as Map
import qualified Data.Text as T
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Externs
import qualified Language.PureScript as P

getReexports :: ModuleOld -> [ExternDecl]
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

replaceReexport :: ExternDecl -> ModuleOld -> ModuleOld -> ModuleOld
replaceReexport e@(Export _) (m, decls) (_, newDecls) =
  (m, filter (/= e) decls `union` newDecls)
replaceReexport _ _ _ = P.internalError "Should only get Exports here"

emptyModule :: ModuleOld
emptyModule = ("Empty", [])

isExport :: ExternDecl -> Bool
isExport (Export _) = True
isExport _ = False

removeExportDecls :: ModuleOld -> ModuleOld
removeExportDecls = fmap (filter (not . isExport))

replaceReexports :: ModuleOld -> Map ModuleIdent [ExternDecl] -> ModuleOld
replaceReexports m db = result
  where
    reexports = getReexports m
    result = foldl go (removeExportDecls m) reexports

    go :: ModuleOld -> ExternDecl -> ModuleOld
    go m' re@(Export name) = replaceReexport re m' (getModule name)
    go _ _ = P.internalError "Should only get Exports here"

    getModule :: ModuleIdent -> ModuleOld
    getModule name = clean res
      where
        res = fromMaybe emptyModule $ (name , ) <$> Map.lookup name db
        -- we have to do this because keeping self exports in will result in
        -- infinite loops
        clean (mn, decls) = (mn,) (filter (/= Export mn) decls)

resolveReexports :: Map ModuleIdent [ExternDecl] -> ModuleOld -> ModuleOld
resolveReexports modules m =
  let replaced = replaceReexports m modules
  in if null (getReexports replaced)
     then replaced
     else resolveReexports modules replaced

resolveReexports2 :: Map T.Text [ExternDecl] -> ModuleOld -> Module
resolveReexports2 decls = convertModule . resolveReexports decls
