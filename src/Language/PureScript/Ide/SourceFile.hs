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

module Language.PureScript.Ide.SourceFile where

import           Protolude

import qualified Language.PureScript                  as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Util
import           Language.PureScript.Ide.Externs      (unwrapPositioned,
                                                       unwrapPositionedRef)
import           Language.PureScript.Ide.Types
import           System.Directory
import           System.FilePath
import           System.IO.UTF8                       (readUTF8File)

parseModuleFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                       FilePath -> m P.Module
parseModuleFromFile fp = do
  exists <- liftIO (doesFileExist fp)
  if exists
    then do
      content <- liftIO (readUTF8File fp)
      let m = do tokens <- P.lex fp content
                 P.runTokenParser "" P.parseModule tokens
      either (throwError . (`ParseError` "File could not be parsed.")) pure m
    else throwError (NotFound "File does not exist.")

-- data Module = Module SourceSpan [Comment] ModuleName [Declaration] (Maybe [DeclarationRef])

getDeclarations :: P.Module -> [P.Declaration]
getDeclarations (P.Module _ _ _ declarations _) = declarations

getImports :: P.Module -> [(P.ModuleName, P.ImportDeclarationType, Maybe P.ModuleName)]
getImports (P.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport (P.PositionedDeclaration _ _ (P.ImportDeclaration a b c)) = Just (a, b, c)
    isImport _ = Nothing

getImportsForFile :: (MonadIO m, MonadError PscIdeError m) =>
                     FilePath -> m [ModuleImport]
getImportsForFile fp = do
  module' <- parseModuleFromFile fp
  let imports = getImports module'
  pure (mkModuleImport . unwrapPositionedImport <$> imports)
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

getPositionedImports :: P.Module -> [P.Declaration]
getPositionedImports (P.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport i@(P.PositionedDeclaration _ _ P.ImportDeclaration{}) = Just i
    isImport _ = Nothing

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

goToDefinition :: Text -> FilePath -> IO (Maybe P.SourceSpan)
goToDefinition q fp = do
  m <- runExceptT (parseModuleFromFile fp)
  case m of
    Right module' -> pure (getDeclPosition module' q)
    Left _ -> pure Nothing
