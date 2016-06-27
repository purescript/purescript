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
-- Generally useful functions
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.Ide.Util
  ( identifierFromIdeDeclaration
  , unwrapMatch
  , completionFromMatch
  , infoFromMatch
  , encodeT
  , decodeT
  , discardAstInfo
  , module Language.PureScript.Ide.Conversions
  ) where

import           Protolude
import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding       (decodeUtf8, encodeUtf8)
import qualified Language.PureScript           as P
import           Language.PureScript.Ide.Types
import           Language.PureScript.Ide.Conversions

identifierFromIdeDeclaration :: IdeDeclaration -> Text
identifierFromIdeDeclaration d = case d of
  IdeValue name _ -> name
  IdeType name _ -> runProperNameT name
  IdeTypeSynonym name _ -> runProperNameT name
  IdeDataConstructor name _ _ -> name
  IdeTypeClass name -> runProperNameT name
  IdeValueOperator op _ _ _ -> runOpNameT op
  IdeTypeOperator op _ _ _ -> runOpNameT op

discardAstInfo :: IdeDeclarationAnn -> IdeDeclaration
discardAstInfo (IdeDeclarationAnn _ d) = d

unwrapMatch :: Match a -> a
unwrapMatch (Match (_, ed)) = ed

completionFromMatch :: Match IdeDeclaration -> Completion
completionFromMatch = Completion . completionFromMatch'

completionFromMatch' :: Match IdeDeclaration -> (Text, Text, Text)
completionFromMatch' (Match (m', d)) = case d of
  IdeValue name type' -> (m, name, prettyTypeT type')
  IdeType name kind -> (m, runProperNameT name, toS (P.prettyPrintKind kind))
  IdeTypeSynonym name kind -> (m, runProperNameT name, prettyTypeT kind)
  IdeDataConstructor name _ type' -> (m, name, prettyTypeT type')
  IdeTypeClass name -> (m, runProperNameT name, "class")
  IdeValueOperator op ref precedence associativity -> (m, runOpNameT op, showFixity precedence associativity ref op)
  IdeTypeOperator op ref precedence associativity -> (m, runOpNameT op, showFixity precedence associativity ref op)
  where
    m = runModuleNameT m'
    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, show p, r, "as", runOpNameT o]

infoFromMatch :: Match IdeDeclarationAnn -> Info
infoFromMatch (Match (m, (IdeDeclarationAnn ss d))) =
  Info (a, b, c, ss)
  where
    (a, b, c) = completionFromMatch' (Match (m, d))

encodeT :: (ToJSON a) => a -> Text
encodeT = toS . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . toS
