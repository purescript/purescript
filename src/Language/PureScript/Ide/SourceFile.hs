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

import           Prelude

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Maybe                           (mapMaybe)
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Language.PureScript.AST.Declarations as D
import qualified Language.PureScript.AST.SourcePos    as SP
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Externs      (unwrapPositioned,
                                                       unwrapPositionedRef)
import           Language.PureScript.Ide.Types
import qualified Language.PureScript.Names            as N
import qualified Language.PureScript.Parser           as P
import           System.Directory
import           System.IO.UTF8                       (readUTF8File)

parseModuleFromFile :: (MonadIO m, MonadError PscIdeError m) =>
                       FilePath -> m D.Module
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

getDeclarations :: D.Module -> [D.Declaration]
getDeclarations (D.Module _ _ _ declarations _) = declarations

getImports :: D.Module -> [D.Declaration]
getImports (D.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport (D.PositionedDeclaration _ _ (i@D.ImportDeclaration{})) = Just i
    isImport _ = Nothing

getImportsForFile :: (MonadIO m, MonadError PscIdeError m) =>
                     FilePath -> m [ModuleImport]
getImportsForFile fp = do
  module' <- parseModuleFromFile fp
  let imports = getImports module'
  pure (mkModuleImport . unwrapPositionedImport <$> imports)
  where
    mkModuleImport (D.ImportDeclaration mn importType' qualifier) =
      ModuleImport
      (T.pack (N.runModuleName mn))
      importType'
      (T.pack . N.runModuleName <$> qualifier)
    mkModuleImport _ = error "Shouldn't have gotten anything but Imports here"
    unwrapPositionedImport (D.ImportDeclaration mn importType' qualifier) =
      D.ImportDeclaration mn (unwrapImportType importType') qualifier
    unwrapPositionedImport x = x
    unwrapImportType (D.Explicit decls) = D.Explicit (map unwrapPositionedRef decls)
    unwrapImportType (D.Hiding decls)   = D.Hiding (map unwrapPositionedRef decls)
    unwrapImportType D.Implicit         = D.Implicit

getPositionedImports :: D.Module -> [D.Declaration]
getPositionedImports (D.Module _ _ _ declarations _) =
  mapMaybe isImport declarations
  where
    isImport i@(D.PositionedDeclaration _ _ D.ImportDeclaration{}) = Just i
    isImport _ = Nothing

getDeclPosition :: D.Module -> String -> Maybe SP.SourceSpan
getDeclPosition m ident = getFirst (foldMap (match ident) decls)
  where
    decls = getDeclarations m
    match q (D.PositionedDeclaration ss _ decl) = First (if go q decl
                                                         then Just ss
                                                         else Nothing)
    match _ _ = First Nothing

    go q (D.DataDeclaration _ name _ constructors)  =
      properEqual name q || any (\(x,_) -> properEqual x q) constructors
    go q (D.DataBindingGroupDeclaration decls')      = any (go q) decls'
    go q (D.TypeSynonymDeclaration name _ _)        = properEqual name q
    go q (D.TypeDeclaration ident' _)               = identEqual ident' q
    go q (D.ValueDeclaration ident' _ _ _)          = identEqual ident' q
    go q (D.ExternDeclaration ident' _)             = identEqual ident' q
    go q (D.ExternDataDeclaration name _)           = properEqual name q
    go q (D.TypeClassDeclaration name _ _ members)  =
      properEqual name q || any (go q . unwrapPositioned) members
    go q (D.TypeInstanceDeclaration ident' _ _ _ _) =
      identEqual ident' q
    go _ _ = False

    properEqual x q = N.runProperName x == q
    identEqual x q = N.runIdent x == q

goToDefinition :: String -> FilePath -> IO (Maybe SP.SourceSpan)
goToDefinition q fp = do
  m <- runExceptT (parseModuleFromFile fp)
  case m of
    Right module' -> pure (getDeclPosition module' q)
    Left _ -> pure Nothing
