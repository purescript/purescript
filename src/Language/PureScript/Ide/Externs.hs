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

{-# LANGUAGE OverloadedStrings     #-}

module Language.PureScript.Ide.Externs
  ( ExternDecl(..),
    ModuleIdent,
    DeclIdent,
    readExternFile,
    convertExterns,
    unwrapPositioned,
    unwrapPositionedRef
  ) where

import           Prelude                       ()
import           Prelude.Compat

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Data.Aeson                    (decodeStrict)
import           Data.List                     (nub)
import           Data.Maybe                    (mapMaybe)
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import           Language.PureScript.Ide.Error (PscIdeError (..))
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Util

import qualified Language.PureScript           as P

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m P.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " ++ fp ++ " failed"
     Just externs -> pure externs

moduleNameToText :: P.ModuleName -> Text
moduleNameToText = T.pack . P.runModuleName

identToText :: P.Ident -> Text
identToText  = T.pack . P.runIdent

convertExterns :: P.ExternsFile -> Module
convertExterns ef = (moduleName, exportDecls ++ importDecls ++ decls ++ operatorDecls ++ tyOperatorDecls)
  where
    moduleName = moduleNameToText (P.efModuleName ef)
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
  (moduleNameToText (P.eiModule ei))
  []
  (moduleNameToText <$> P.eiImportedAs ei)

convertExport :: P.DeclarationRef -> Maybe ExternDecl
convertExport (P.ModuleRef mn) = Just (Export (moduleNameToText mn))
convertExport _ = Nothing

convertDecl :: P.ExternsDeclaration -> Maybe ExternDecl
convertDecl P.EDType{..} = Just $ TypeDeclaration edTypeName edTypeKind
convertDecl P.EDTypeSynonym{..} = Just $
  TypeSynonymDeclaration edTypeSynonymName edTypeSynonymType
convertDecl P.EDDataConstructor{..} = Just $
  DataConstructor (runProperNameT edDataCtorName) edDataCtorTypeCtor edDataCtorType
convertDecl P.EDValue{..} = Just $
  ValueDeclaration (identToText edValueName) edValueType
convertDecl P.EDClass{..} = Just $ TypeClassDeclaration edClassName
convertDecl P.EDInstance{} = Nothing

convertOperator :: P.ExternsFixity -> ExternDecl
convertOperator P.ExternsFixity{..} =
  ValueOperator
    efOperator
    (T.pack (P.showQualified (either P.runIdent P.runProperName) efAlias))
    efPrecedence
    efAssociativity

convertTypeOperator :: P.ExternsTypeFixity -> ExternDecl
convertTypeOperator P.ExternsTypeFixity{..} =
  TypeOperator
    efTypeOperator
    (T.pack (P.showQualified P.runProperName efTypeAlias))
    efTypePrecedence
    efTypeAssociativity

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = x
unwrapPositionedRef x = x
