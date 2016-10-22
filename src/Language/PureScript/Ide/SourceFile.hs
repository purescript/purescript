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

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.SourceFile
  ( parseModule
  , getImportsForFile
  , extractAstInformation
  -- for tests
  , extractSpans
  , extractTypeAnnotations
  ) where

import           Protolude

import qualified Data.Map                      as Map
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util
import           System.IO.UTF8                (readUTF8File)

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

-- | Extracts AST information from a parsed module
extractAstInformation
  :: P.Module
  -> (DefinitionSites P.SourceSpan, TypeAnnotations)
extractAstInformation (P.Module ss _ _ decls _) =
  let definitions = Map.fromList (concatMap (extractSpans ss) decls)
      typeAnnotations = Map.fromList (extractTypeAnnotations decls)
  in (definitions, typeAnnotations)

-- | Extracts type annotations for functions from a given Module
extractTypeAnnotations
  :: [P.Declaration]
  -> [(P.Ident, P.Type)]
extractTypeAnnotations = mapMaybe extract
  where
    extract d = case unwrapPositioned d of
      P.TypeDeclaration ident ty -> Just (ident, ty)
      _ -> Nothing

-- | Given a surrounding Sourcespan and a Declaration from the PS AST, extracts
-- definition sites inside that Declaration.
extractSpans
  :: P.SourceSpan
  -- ^ The surrounding span
  -> P.Declaration
  -- ^ The declaration to extract spans from
  -> [(Either Text Text, P.SourceSpan)]
  -- ^ A @Right@ corresponds to a type level declaration, and a @Left@ to a
  -- value level one
extractSpans ss d = case d of
  P.PositionedDeclaration ss' _ d' ->
    extractSpans ss' d'
  P.ValueDeclaration i _ _ _ ->
    [(Left (runIdentT i), ss)]
  P.TypeSynonymDeclaration name _ _ ->
    [(Right (runProperNameT name), ss)]
  P.TypeClassDeclaration name _ _ _ members ->
    (Right (runProperNameT name), ss) : concatMap (extractSpans' ss) members
  P.DataDeclaration _ name _ ctors ->
    (Right (runProperNameT name), ss)
    : map (\(cname, _) -> (Left (runProperNameT cname), ss)) ctors
  P.ExternDeclaration ident _ ->
    [(Left (runIdentT ident), ss)]
  P.ExternDataDeclaration name _ ->
    [(Right (runProperNameT name), ss)]
  _ -> []
  where
    -- We need this special case to be able to also get the position info for
    -- typeclass member functions. Typedeclaratations would clash with value
    -- declarations for non-typeclass members, which is why we can't handle them
    -- in extractSpans.
    extractSpans' ssP dP = case dP of
      P.PositionedDeclaration ssP' _ dP' ->
        extractSpans' ssP' dP'
      P.TypeDeclaration ident _ ->
        [(Left (runIdentT ident), ssP)]
      _ -> []
