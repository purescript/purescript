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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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
import qualified Language.PureScript.Externs   as PE

readExternFile :: (MonadIO m, MonadError PscIdeError m) =>
                  FilePath -> m PE.ExternsFile
readExternFile fp = do
   parseResult <- liftIO (decodeStrict <$> BS.readFile fp)
   case parseResult of
     Nothing -> throwError . GeneralError $ "Parsing the extern at: " ++ fp ++ " failed"
     Just externs -> pure externs

moduleNameToText :: P.ModuleName -> Text
moduleNameToText = T.pack . P.runModuleName

identToText :: P.Ident -> Text
identToText  = T.pack . P.runIdent

convertExterns :: PE.ExternsFile -> Module
convertExterns ef = (moduleName, exportDecls ++ importDecls ++ decls)
  where
    moduleName = moduleNameToText (PE.efModuleName ef)
    importDecls = convertImport <$> PE.efImports ef
    exportDecls = mapMaybe (convertExport . unwrapPositionedRef) (PE.efExports ef)
    -- Ignoring operator fixities for now since we're not using them
    -- operatorDecls = convertOperator <$> PE.efFixities ef
    otherDecls = mapMaybe convertDecl (PE.efDeclarations ef)

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

convertImport :: PE.ExternsImport -> ExternDecl
convertImport ei = Dependency
  (moduleNameToText (PE.eiModule ei))
  []
  (moduleNameToText <$> PE.eiImportedAs ei)

convertExport :: P.DeclarationRef -> Maybe ExternDecl
convertExport (P.ModuleRef mn) = Just (Export (moduleNameToText mn))
convertExport _ = Nothing

convertDecl :: PE.ExternsDeclaration -> Maybe ExternDecl
convertDecl PE.EDType{..} = Just $ TypeDeclaration edTypeName edTypeKind
convertDecl PE.EDTypeSynonym{..} = Just $
  TypeSynonymDeclaration edTypeSynonymName edTypeSynonymType
convertDecl PE.EDDataConstructor{..} = Just $
  DataConstructor (runProperNameT edDataCtorName) edDataCtorTypeCtor edDataCtorType
convertDecl PE.EDValue{..} = Just $
  ValueDeclaration (identToText edValueName) edValueType
convertDecl PE.EDClass{..} = Just $ TypeClassDeclaration edClassName
convertDecl PE.EDInstance{} = Nothing

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = x
unwrapPositionedRef x = x
