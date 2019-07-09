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

module Language.PureScript.Ide.SourceFile
  ( parseModule
  , parseModulesFromFiles
  , extractAstInformation
  -- for tests
  , extractSpans
  , extractTypeAnnotations
  ) where

import           Protolude

import           Control.Parallel.Strategies (withStrategy, parList, rseq)
import qualified Data.Map                      as Map
import qualified Language.PureScript           as P
import qualified Language.PureScript.CST       as CST
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

parseModule
  :: (MonadIO m, MonadError IdeError m)
  => FilePath
  -> m (Either FilePath (FilePath, P.Module))
parseModule path = do
  (absPath, contents) <- ideReadFile path
  pure (parseModule' absPath contents)

parseModule' :: FilePath -> Text -> Either FilePath (FilePath, P.Module)
parseModule' path file =
  case CST.parseFromFile path file of
    Left _ -> Left path
    Right m -> Right (path, m)

parseModulesFromFiles
  :: (MonadIO m, MonadError IdeError m)
  => [FilePath]
  -> m [Either FilePath (FilePath, P.Module)]
parseModulesFromFiles paths = do
  files <- traverse ideReadFile paths
  pure (inParallel (map (uncurry parseModule') files))
  where
    inParallel :: [Either e (k, a)] -> [Either e (k, a)]
    inParallel = withStrategy (parList rseq)

-- | Extracts AST information from a parsed module
extractAstInformation
  :: P.Module
  -> (DefinitionSites P.SourceSpan, TypeAnnotations)
extractAstInformation (P.Module moduleSpan _ mn decls _) =
  let definitions =
        Map.insert
          (IdeNamespaced IdeNSModule (P.runModuleName mn)) moduleSpan
          (Map.fromList (concatMap extractSpans decls))
      typeAnnotations = Map.fromList (extractTypeAnnotations decls)
  in (definitions, typeAnnotations)

-- | Extracts type annotations for functions from a given Module
extractTypeAnnotations :: [P.Declaration] -> [(P.Ident, P.SourceType)]
extractTypeAnnotations = mapMaybe (map P.unwrapTypeDeclaration . P.getTypeDeclaration)

-- | Given a surrounding Sourcespan and a Declaration from the PS AST, extracts
-- definition sites inside that Declaration.
extractSpans
  :: P.Declaration
  -- ^ The declaration to extract spans from
  -> [(IdeNamespaced, P.SourceSpan)]
  -- ^ Declarations and their source locations
extractSpans d = case d of
  P.ValueDecl (ss, _) i _ _ _ ->
    [(IdeNamespaced IdeNSValue (P.runIdent i), ss)]
  P.TypeSynonymDeclaration (ss, _) name _ _ ->
    [(IdeNamespaced IdeNSType (P.runProperName name), ss)]
  P.TypeClassDeclaration (ss, _) name _ _ _ members ->
    (IdeNamespaced IdeNSType (P.runProperName name), ss) : concatMap extractSpans' members
  P.DataDeclaration (ss, _) _ name _ ctors ->
    (IdeNamespaced IdeNSType (P.runProperName name), ss) : map dtorSpan ctors
  P.FixityDeclaration (ss, _) (Left (P.ValueFixity _ _ opName)) ->
    [(IdeNamespaced IdeNSValue (P.runOpName opName), ss)]
  P.FixityDeclaration (ss, _) (Right (P.TypeFixity _ _ opName)) ->
    [(IdeNamespaced IdeNSType (P.runOpName opName), ss)]
  P.ExternDeclaration (ss, _) ident _ ->
    [(IdeNamespaced IdeNSValue (P.runIdent ident), ss)]
  P.ExternDataDeclaration (ss, _) name _ ->
    [(IdeNamespaced IdeNSType (P.runProperName name), ss)]
  P.ExternKindDeclaration (ss, _) name ->
    [(IdeNamespaced IdeNSKind (P.runProperName name), ss)]
  _ -> []
  where
    dtorSpan :: P.DataConstructorDeclaration -> (IdeNamespaced, P.SourceSpan)
    dtorSpan P.DataConstructorDeclaration{ P.dataCtorName = name, P.dataCtorAnn = (ss, _) } =
      (IdeNamespaced IdeNSValue (P.runProperName name), ss)
    -- We need this special case to be able to also get the position info for
    -- typeclass member functions. Typedeclarations would clash with value
    -- declarations for non-typeclass members, which is why we can't handle them
    -- in extractSpans.
    extractSpans' dP = case dP of
      P.TypeDeclaration (P.TypeDeclarationData (ss', _) ident _) ->
        [(IdeNamespaced IdeNSValue (P.runIdent ident), ss')]
      _ -> []
