-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Util
-- Description : Generally useful functions and conversions
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Generally useful functions and conversions
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.Util where

import           Data.Aeson
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lazy                (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Types

runProperNameT :: P.ProperName a -> Text
runProperNameT = T.pack . P.runProperName

runIdentT :: P.Ident -> Text
runIdentT = T.pack . P.runIdent

prettyTypeT :: P.Type -> Text
prettyTypeT = T.unwords . fmap T.strip . T.lines . T.pack . P.prettyPrintType

identifierFromExternDecl :: ExternDecl -> Text
identifierFromExternDecl (ValueDeclaration name _) = name
identifierFromExternDecl (TypeDeclaration name _) = runProperNameT name
identifierFromExternDecl (TypeSynonymDeclaration name _) = runProperNameT name
identifierFromExternDecl (DataConstructor name _ _) = name
identifierFromExternDecl (TypeClassDeclaration name) = runProperNameT name
identifierFromExternDecl (ModuleDecl name _) = name
identifierFromExternDecl Dependency{} = "~Dependency~"
identifierFromExternDecl Export{} = "~Export~"

identifierFromMatch :: Match -> Text
identifierFromMatch (Match _ ed) = identifierFromExternDecl ed

completionFromMatch :: Match -> Maybe Completion
completionFromMatch (Match _ Dependency{}) = Nothing
completionFromMatch (Match _ Export{}) = Nothing
completionFromMatch (Match m d) = Just $ case d of
  ValueDeclaration name type' -> Completion (m, name, prettyTypeT type')
  TypeDeclaration name kind -> Completion (m, runProperNameT name, T.pack $ P.prettyPrintKind kind)
  TypeSynonymDeclaration name kind -> Completion (m, runProperNameT name, prettyTypeT kind)
  DataConstructor name _ type' -> Completion (m, name, prettyTypeT type')
  TypeClassDeclaration name -> Completion (m, runProperNameT name, "class")
  ModuleDecl name _ -> Completion ("module", name, "module")
  _ -> error "the impossible happened in completionFromMatch"

encodeT :: (ToJSON a) => a -> Text
encodeT = toStrict . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . fromStrict
