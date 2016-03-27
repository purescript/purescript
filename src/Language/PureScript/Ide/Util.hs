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

import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Types

import Data.Aeson
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

runProperNameT :: P.ProperName a -> Text
runProperNameT = T.pack . P.runProperName

runIdentT :: P.Ident -> Text
runIdentT = T.pack . P.runIdent

prettyTypeT :: P.Type -> Text
prettyTypeT = T.unwords . fmap T.strip . T.lines . T.pack . P.prettyPrintType

identifierFromExternDecl :: ExternDecl -> Text
identifierFromExternDecl (ValueDeclaration name _) = name
identifierFromExternDecl (TypeDeclaration name _) = runProperNameT name
identifierFromExternDecl (DataConstructor name _ _) = name
identifierFromExternDecl (TypeClassDeclaration name) = runProperNameT name
identifierFromExternDecl (ModuleDecl name _) = name
identifierFromExternDecl Dependency{} = "~Dependency~"
identifierFromExternDecl Export{} = "~Export~"

identifierFromMatch :: Match -> Text
identifierFromMatch (Match _ ed) = identifierFromExternDecl ed

completionFromMatch :: Match -> Completion
completionFromMatch (Match m (ValueDeclaration name type')) =
  Completion (m, name, prettyTypeT type')
completionFromMatch (Match m (TypeDeclaration name kind)) =
  Completion (m, runProperNameT name, T.pack $ P.prettyPrintKind kind)
completionFromMatch (Match m (DataConstructor name _ type')) =
  Completion (m, name, prettyTypeT type')
completionFromMatch (Match m (TypeClassDeclaration name)) =
  Completion (m, runProperNameT name, "class")
completionFromMatch (Match _ (ModuleDecl name _)) =
  Completion ("module", name, "module")
completionFromMatch (Match _ Dependency{}) =
  error "Can't make a Completion from a Dependency"
completionFromMatch (Match _ Export{}) =
  error "Can't make a Completion from an Export"

encodeT :: (ToJSON a) => a -> Text
encodeT = toStrict . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . fromStrict
