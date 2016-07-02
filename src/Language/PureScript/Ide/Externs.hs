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
  ( ExternDecl(..),
    ModuleIdent,
    readExternFile,
    convertExterns,
    convertModule,
    unwrapPositioned,
    unwrapPositionedRef
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

convertExterns :: P.ExternsFile -> ModuleOld
convertExterns ef = (runModuleNameT moduleName, exportDecls ++ importDecls ++ decls ++ operatorDecls ++ tyOperatorDecls)
  where
    moduleName = P.efModuleName ef
    importDecls = convertImport <$> P.efImports ef
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (P.efExports ef)
    operatorDecls = convertOperator <$> P.efFixities ef
    tyOperatorDecls = convertTypeOperator <$> P.efTypeFixities ef
    otherDecls = mapMaybe convertDecl (P.efDeclarations ef)

    typeClassFilter = foldMap removeTypeDeclarationsForClass (filter isTypeClassDeclaration otherDecls)
    decls = nub $ appEndo typeClassFilter otherDecls

removeTypeDeclarationsForClass :: ExternDecl -> Endo [ExternDecl]
removeTypeDeclarationsForClass (TypeClassDeclaration n) = Endo (filter notDuplicate)
  where notDuplicate (TypeDeclaration n' _) = runProperNameT n /= runProperNameT n'
        notDuplicate (TypeSynonymDeclaration n' _) = runProperNameT n /= runProperNameT n'
        notDuplicate _ = True
removeTypeDeclarationsForClass _ = mempty

isTypeClassDeclaration :: ExternDecl -> Bool
isTypeClassDeclaration TypeClassDeclaration{} = True
isTypeClassDeclaration _ = False

convertImport :: P.ExternsImport -> ExternDecl
convertImport ei = Dependency
  (runModuleNameT (P.eiModule ei))
  []
  (runModuleNameT <$> P.eiImportedAs ei)

convertExport :: P.DeclarationRef -> Maybe ExternDecl
convertExport (P.ModuleRef mn) = Just (Export (runModuleNameT mn))
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Maybe ExternDecl
convertDecl P.EDType{..} = Just $ TypeDeclaration edTypeName edTypeKind
convertDecl P.EDTypeSynonym{..} = Just $
  TypeSynonymDeclaration edTypeSynonymName edTypeSynonymType
convertDecl P.EDDataConstructor{..} = Just $
  DataConstructor (runProperNameT edDataCtorName) edDataCtorTypeCtor edDataCtorType
convertDecl P.EDValue{..} = Just $
  ValueDeclaration (runIdentT edValueName) edValueType
convertDecl P.EDClass{..} = Just $ TypeClassDeclaration edClassName
convertDecl P.EDInstance{} = Nothing

convertOperator :: P.ExternsFixity -> ExternDecl
convertOperator P.ExternsFixity{..} =
  ValueOperator
    efOperator
    (toS (P.showQualified (either P.runIdent P.runProperName) efAlias))
    efPrecedence
    efAssociativity

convertTypeOperator :: P.ExternsTypeFixity -> ExternDecl
convertTypeOperator P.ExternsTypeFixity{..} =
  TypeOperator
    efTypeOperator
    (toS (P.showQualified P.runProperName efTypeAlias))
    efTypePrecedence
    efTypeAssociativity

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = x
unwrapPositionedRef x = x

convertModule :: Map (Either Text Text) P.SourceSpan -> ModuleOld -> Module
convertModule ast (mn, decls) =
  (P.moduleNameFromString (toS mn), mapMaybe convertDeclaration decls)
  where convertDeclaration :: ExternDecl -> Maybe IdeDeclarationAnn
        convertDeclaration d = case d of
          ValueDeclaration i t ->
            annotateValue i (IdeValue i t)
          TypeDeclaration i k ->
            annotateType (runProperNameT i) (IdeType i k)
          TypeSynonymDeclaration i t ->
            annotateType (runProperNameT i) (IdeTypeSynonym i t)
          DataConstructor i tn t ->
            annotateValue i (IdeDataConstructor i tn t)
          TypeClassDeclaration i ->
            annotateType (runProperNameT i) (IdeTypeClass i)
          ValueOperator n i p a ->
            annotateValue i (IdeValueOperator n i p a)
          TypeOperator n i p a ->
            annotateType i (IdeTypeOperator n i p a)
          Dependency{} -> Nothing
          ModuleDecl _ _ -> Nothing
          Export _ -> Nothing
        annotateValue x = Just . IdeDeclarationAnn (Map.lookup (Left x) ast)
        annotateType x = Just . IdeDeclarationAnn (Map.lookup (Right x) ast)
