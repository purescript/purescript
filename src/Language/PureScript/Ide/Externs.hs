-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Externs
-- Description : Handles externs files for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Handles externs files for psc-ide
-----------------------------------------------------------------------------

{-# LANGUAGE PackageImports  #-}

module Language.PureScript.Ide.Externs
  ( readExternFile
  , convertExterns
  ) where

import           Protolude hiding (to, from, (&))

import           Control.Lens
import           "monad-logger" Control.Monad.Logger
import           Data.Aeson (decodeStrict)
import           Data.Aeson.Types (withObject, parseMaybe, (.:))
import qualified Data.ByteString as BS
import           Data.Version (showVersion)
import           Language.PureScript.Ide.Error (IdeError (..))
import           Language.PureScript.Ide.Types

import qualified Language.PureScript as P

readExternFile
  :: (MonadIO m, MonadError IdeError m, MonadLogger m)
  => FilePath
  -> m P.ExternsFile
readExternFile fp = do
   externsFile <- liftIO (BS.readFile fp)
   case decodeStrict externsFile of
     Nothing ->
       let parser = withObject "ExternsFileVersion" $ \o -> o .: "efVersion"
           maybeEFVersion = parseMaybe parser =<< decodeStrict externsFile
       in case maybeEFVersion of
         Nothing ->
            throwError (GeneralError
                        ("Parsing the extern at: " <> toS fp <> " failed"))
         Just efVersion -> do
           let errMsg = "Version mismatch for the externs at: " <> toS fp
                        <> " Expected: " <> version
                        <> " Found: " <> efVersion
           logErrorN errMsg
           throwError (GeneralError errMsg)
     Just externs -> pure externs

     where
       version = toS (showVersion P.version)

convertExterns :: P.ExternsFile -> ([IdeDeclarationAnn], [(P.ModuleName, P.DeclarationRef)])
convertExterns ef =
  (decls, exportDecls)
  where
    decls = map
      (IdeDeclarationAnn emptyAnn)
      (resolvedDeclarations <> operatorDecls <> tyOperatorDecls)
    exportDecls = mapMaybe convertExport (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    (toResolve, declarations) =
      second catMaybes (partitionEithers (map convertDecl (P.efDeclarations ef)))

    -- It's important that we resolve synonyms first, because that resolving
    -- process removes the corresponding type declarations. This way we don't
    -- leave any stray type declarations for type classes around since they have
    -- already been cleaned up in the type synonym pass.
    resolver = resolveTypeClasses toResolve <> resolveSynonyms toResolve
    resolvedDeclarations = appEndo resolver declarations

resolveSynonyms :: [ToResolve] -> Endo [IdeDeclaration]
resolveSynonyms = foldMap resolveSynonym
  where
    resolveSynonym tr = case tr of
      TypeClassToResolve _ -> mempty
      SynonymToResolve tn ty -> Endo $ \decls ->
        case findType tn decls of
          Nothing -> decls
          Just tyDecl ->
            IdeDeclTypeSynonym (IdeTypeSynonym tn ty (tyDecl^.ideTypeKind))
            : filter (not . anyOf (_IdeDeclType.ideTypeName) (== tn)) decls

resolveTypeClasses :: [ToResolve] -> Endo [IdeDeclaration]
resolveTypeClasses = foldMap resolveTypeClass
  where
    resolveTypeClass tr = case tr of
      SynonymToResolve _ _ -> mempty
      TypeClassToResolve tcn -> Endo $ \decls ->
        case findSynonym (P.coerceProperName tcn) decls of
          Nothing -> decls
          Just tySyn -> IdeDeclTypeClass
            (IdeTypeClass tcn (tySyn^.ideSynonymKind) [])
            : filter (not . anyOf (_IdeDeclTypeSynonym.ideSynonymName) (== P.coerceProperName tcn)) decls

findType :: P.ProperName 'P.TypeName -> [IdeDeclaration] -> Maybe IdeType
findSynonym :: P.ProperName 'P.TypeName -> [IdeDeclaration] -> Maybe IdeTypeSynonym
(findType, findSynonym) = ( findDecl _IdeDeclType ideTypeName
                          , findDecl _IdeDeclTypeSynonym ideSynonymName
                          )
  where
    findDecl p l tn decls = decls
      & mapMaybe (preview p)
      & find ((==) tn . view l)

-- The Externs format splits information about synonyms across EDType and
-- EDTypeSynonym declarations. For type classes there are three declarations
-- involved. We collect these and resolve them at the end of the conversion process.
data ToResolve
  = TypeClassToResolve (P.ProperName 'P.ClassName)
  | SynonymToResolve (P.ProperName 'P.TypeName) P.Type

convertExport :: P.DeclarationRef -> Maybe (P.ModuleName, P.DeclarationRef)
convertExport (P.ReExportRef _ m r) = Just (m, r)
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Either ToResolve (Maybe IdeDeclaration)
convertDecl ed = case ed of
  P.EDType{..} ->
    Right (Just (IdeDeclType (IdeType edTypeName edTypeKind [])))
  P.EDTypeSynonym{..} ->
    Left (SynonymToResolve edTypeSynonymName edTypeSynonymType)
  P.EDDataConstructor{..} ->
    Right
      (Just
        (IdeDeclDataConstructor
          (IdeDataConstructor edDataCtorName edDataCtorTypeCtor edDataCtorType)))
  P.EDValue{..} ->
    Right (Just (IdeDeclValue (IdeValue edValueName edValueType)))
  P.EDClass{..} ->
    Left (TypeClassToResolve edClassName)
  P.EDKind{..} ->
    Right (Just (IdeDeclKind edKindName))
  P.EDInstance{} ->
    Right Nothing

convertOperator :: P.ExternsFixity -> IdeDeclaration
convertOperator P.ExternsFixity{..} =
  IdeDeclValueOperator
    (IdeValueOperator
      efOperator
      efAlias
      efPrecedence
      efAssociativity
      Nothing)

convertTypeOperator :: P.ExternsTypeFixity -> IdeDeclaration
convertTypeOperator P.ExternsTypeFixity{..} =
  IdeDeclTypeOperator
    (IdeTypeOperator
      efTypeOperator
      efTypeAlias
      efTypePrecedence
      efTypeAssociativity
      Nothing)
