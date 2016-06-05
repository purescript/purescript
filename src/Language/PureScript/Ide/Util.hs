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
import           Control.Monad                 (unless)
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

identifierFromIdeDeclaration :: IdeDeclaration -> Text
identifierFromIdeDeclaration d = case d of
  IdeValue name _ -> name
  IdeType name _ -> runProperNameT name
  IdeTypeSynonym name _ -> runProperNameT name
  IdeDataConstructor name _ _ -> name
  IdeTypeClass name -> runProperNameT name
  IdeValueOperator op _ _ _ -> runOpNameT op
  IdeTypeOperator op _ _ _ -> runOpNameT op

identifierFromMatch :: Match -> Text
identifierFromMatch (Match _ ed) = identifierFromIdeDeclaration ed

completionFromMatch :: Match -> Completion
completionFromMatch (Match m' d) = case d of
  IdeValue name type' -> Completion (m, name, prettyTypeT type')
  IdeType name kind -> Completion (m, runProperNameT name, T.pack $ P.prettyPrintKind kind)
  IdeTypeSynonym name kind -> Completion (m, runProperNameT name, prettyTypeT kind)
  IdeDataConstructor name _ type' -> Completion (m, name, prettyTypeT type')
  IdeTypeClass name -> Completion (m, runProperNameT name, "class")
  IdeValueOperator op ref precedence associativity -> Completion (m, runOpNameT op, showFixity precedence associativity ref op)
  IdeTypeOperator op ref precedence associativity -> Completion (m, runOpNameT op, showFixity precedence associativity ref op)
  where
    m = runModuleNameT m'
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

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond act = cond >>= flip unless act
