-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.SourceFile
-- Description : Getting declarations from PureScript sourcefiles
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Getting declarations from PureScript sourcefiles
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings     #-}

module Language.PureScript.Ide.SourceFile
  ( parseModule
  , getImportsForFile
  -- SOON...
  , getDeclPosition
  ) where

import           Protolude

import qualified Language.PureScript                  as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Util
import           Language.PureScript.Ide.Externs      (unwrapPositioned,
                                                       unwrapPositionedRef)
import           Language.PureScript.Ide.Types
import           System.FilePath
import           System.IO.UTF8                       (readUTF8File)

parseModule
  :: (MonadIO m)
  => FilePath
  -> m (Either FilePath (FilePath, P.Module) )
parseModule path = do
  contents <- liftIO (readUTF8File path)
  case P.parseModuleFromFile identity (path, contents) of
    Left _ -> pure (Left path)
    Right m -> pure (Right m)

getImports :: P.Module -> [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)]
getImports (P.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport (P.PositionedDeclaration _ _ (P.ImportDeclaration a b c)) = Just (a, b, c)
    isImport _ = Nothing

getImportsForFile :: (MonadIO m, MonadError PscIdeError m) =>
                     FilePath -> m [ModuleImport]
getImportsForFile fp = do
  moduleE <- parseModule fp
  case moduleE of
    Left _ -> throwError (GeneralError "Failed to parse sourcefile.")
    Right (_, module') ->
      pure (mkModuleImport . unwrapPositionedImport <$> getImports module')
      where
        mkModuleImport (mn, importType', qualifier) =
          ModuleImport
          (runModuleNameT mn)
          importType'
          (runModuleNameT <$> qualifier)
        unwrapPositionedImport (mn, it, q) = (mn, unwrapImportType it, q)
        unwrapImportType (P.Explicit decls) = P.Explicit (map unwrapPositionedRef decls)
        unwrapImportType (P.Hiding decls)   = P.Hiding (map unwrapPositionedRef decls)
        unwrapImportType P.Implicit         = P.Implicit

getDeclPosition :: P.Module -> Text -> Maybe P.SourceSpan
getDeclPosition m ident = getFirst (foldMap (match ident) decls)
  where
    decls = getDeclarations m
    match q (P.PositionedDeclaration ss _ decl) = First (if go q decl
                                                         then Just ss
                                                         else Nothing)
    match _ _ = First Nothing

    go q (P.DataDeclaration _ name _ constructors)  =
      properEqual name q || any (\(x,_) -> properEqual x q) constructors
    go q (P.DataBindingGroupDeclaration decls')      = any (go q) decls'
    go q (P.TypeSynonymDeclaration name _ _)        = properEqual name q
    go q (P.TypeDeclaration ident' _)               = identEqual ident' q
    go q (P.ValueDeclaration ident' _ _ _)          = identEqual ident' q
    go q (P.ExternDeclaration ident' _)             = identEqual ident' q
    go q (P.ExternDataDeclaration name _)           = properEqual name q
    go q (P.TypeClassDeclaration name _ _ members)  =
      properEqual name q || any (go q . unwrapPositioned) members
    go q (P.TypeInstanceDeclaration ident' _ _ _ _) =
      identEqual ident' q
    go _ _ = False

    properEqual x q = runProperNameT x == q
    identEqual x q = runIdentT x == q

    getDeclarations :: P.Module -> [P.Declaration]
    getDeclarations (P.Module _ _ _ declarations _) = declarations
