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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Language.PureScript.Ide.Externs
  ( readExternFile,
    convertExterns,
    annotateLocations
  ) where

import           Protolude

import           Data.Aeson                    (decodeStrict)
import           Data.List                     (nub)
import qualified Data.Map                      as Map
import qualified Data.ByteString               as BS
import           Language.PureScript.Ide.Error (PscIdeError (..))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import qualified Language.PureScript           as P
import           System.FilePath

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m P.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " <> toS fp <> " failed"
     Just externs -> pure externs

convertExterns :: P.ExternsFile -> (Module, [(P.ModuleName, P.DeclarationRef)])
convertExterns ef =
  ((P.efModuleName ef, decls), exportDecls)
  where
    decls = map
      (IdeDeclarationAnn emptyAnn)
      (cleanDeclarations ++ operatorDecls ++ tyOperatorDecls)
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    declarations = mapMaybe convertDecl (P.efDeclarations ef)

    typeClassFilter = foldMap removeTypeDeclarationsForClass (filter isTypeClassDeclaration declarations)
    cleanDeclarations = nub $ appEndo typeClassFilter declarations

removeTypeDeclarationsForClass :: IdeDeclaration -> Endo [IdeDeclaration]
removeTypeDeclarationsForClass (IdeTypeClass n) = Endo (filter notDuplicate)
  where notDuplicate (IdeType n' _) = runProperNameT n /= runProperNameT n'
        notDuplicate (IdeTypeSynonym n' _) = runProperNameT n /= runProperNameT n'
        notDuplicate _ = True
removeTypeDeclarationsForClass _ = mempty

isTypeClassDeclaration :: IdeDeclaration -> Bool
isTypeClassDeclaration IdeTypeClass{} = True
isTypeClassDeclaration _ = False

convertExport :: P.DeclarationRef -> Maybe (P.ModuleName, P.DeclarationRef)
convertExport (P.ReExportRef m r) = Just (m, r)
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Maybe IdeDeclaration
convertDecl P.EDType{..} = Just (IdeType edTypeName edTypeKind)
convertDecl P.EDTypeSynonym{..} =
  Just (IdeTypeSynonym edTypeSynonymName edTypeSynonymType)
convertDecl P.EDDataConstructor{..} = Just $
  IdeDataConstructor edDataCtorName edDataCtorTypeCtor edDataCtorType
convertDecl P.EDValue{..} = Just $
  IdeValue edValueName edValueType
convertDecl P.EDClass{..} = Just (IdeTypeClass edClassName)
convertDecl P.EDInstance{} = Nothing

convertOperator :: P.ExternsFixity -> IdeDeclaration
convertOperator P.ExternsFixity{..} =
  IdeValueOperator
    efOperator
    (toS (P.showQualified (either P.runIdent P.runProperName) efAlias))
    efPrecedence
    efAssociativity

convertTypeOperator :: P.ExternsTypeFixity -> IdeDeclaration
convertTypeOperator P.ExternsTypeFixity{..} =
  IdeTypeOperator
    efTypeOperator
    (toS (P.showQualified P.runProperName efTypeAlias))
    efTypePrecedence
    efTypeAssociativity

annotateLocations :: Map (Either Text Text) P.SourceSpan -> Module -> Module
annotateLocations ast (moduleName, decls) =
  (moduleName, map convertDeclaration decls)
  where
    convertDeclaration :: IdeDeclarationAnn -> IdeDeclarationAnn
    convertDeclaration (IdeDeclarationAnn ann d) = case d of
      IdeValue i t ->
        annotateValue (runIdentT i) (IdeValue i t)
      IdeType i k ->
        annotateType (runProperNameT i) (IdeType i k)
      IdeTypeSynonym i t ->
        annotateType (runProperNameT i) (IdeTypeSynonym i t)
      IdeDataConstructor i tn t ->
        annotateValue (runProperNameT i) (IdeDataConstructor i tn t)
      IdeTypeClass i ->
        annotateType (runProperNameT i) (IdeTypeClass i)
      IdeValueOperator n i p a ->
        annotateValue i (IdeValueOperator n i p a)
      IdeTypeOperator n i p a ->
        annotateType i (IdeTypeOperator n i p a)
      where
        annotateValue x = IdeDeclarationAnn (ann {annLocation = Map.lookup (Left x) ast})
        annotateType x = IdeDeclarationAnn (ann {annLocation = Map.lookup (Right x) ast})
