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

import           Prelude.Compat
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

runOpNameT :: P.OpName a -> Text
runOpNameT = T.pack . P.runOpName

runModuleNameT :: P.ModuleName -> Text
runModuleNameT = T.pack . P.runModuleName

prettyTypeT :: P.Type -> Text
prettyTypeT = T.unwords . fmap T.strip . T.lines . T.pack . P.prettyPrintType

identifierFromExternDecl :: ExternDecl -> Text
identifierFromExternDecl (ValueDeclaration name _) = name
identifierFromExternDecl (TypeDeclaration name _) = runProperNameT name
identifierFromExternDecl (TypeSynonymDeclaration name _) = runProperNameT name
identifierFromExternDecl (DataConstructor name _ _) = name
identifierFromExternDecl (TypeClassDeclaration name) = runProperNameT name
identifierFromExternDecl (ModuleDecl name _) = name
identifierFromExternDecl (ValueOperator op _ _ _) = runOpNameT op
identifierFromExternDecl (TypeOperator op _ _ _) = runOpNameT op
identifierFromExternDecl Dependency{} = "~Dependency~"
identifierFromExternDecl Export{} = "~Export~"

identifierFromMatch :: Match -> Text
identifierFromMatch (Match _ ed) = identifierFromExternDecl ed

completionFromMatch :: Match -> Maybe Completion
completionFromMatch (Match m d) = case d of
  ValueDeclaration name type' -> Just $ Completion (m, name, prettyTypeT type')
  TypeDeclaration name kind -> Just $ Completion (m, runProperNameT name, T.pack $ P.prettyPrintKind kind)
  TypeSynonymDeclaration name kind -> Just $ Completion (m, runProperNameT name, prettyTypeT kind)
  DataConstructor name _ type' -> Just $ Completion (m, name, prettyTypeT type')
  TypeClassDeclaration name -> Just $ Completion (m, runProperNameT name, "class")
  ModuleDecl name _ -> Just $ Completion ("module", name, "module")
  ValueOperator op ref precedence associativity -> Just $ Completion (m, runOpNameT op, showFixity precedence associativity ref op)
  TypeOperator op ref precedence associativity -> Just $ Completion (m, runOpNameT op, showFixity precedence associativity ref op)
  Dependency{} -> Nothing
  Export{} -> Nothing
  where
    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, T.pack (show p), r, "as", runOpNameT o]


encodeT :: (ToJSON a) => a -> Text
encodeT = toStrict . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . fromStrict
