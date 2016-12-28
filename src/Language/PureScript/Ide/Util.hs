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

module Language.PureScript.Ide.Util
  ( identifierFromIdeDeclaration
  , unwrapMatch
  , unwrapPositioned
  , unwrapPositionedRef
  , completionFromMatch
  , encodeT
  , decodeT
  , discardAnn
  , withEmptyAnn
  , valueOperatorAliasT
  , typeOperatorAliasT
  , module Language.PureScript.Ide.Conversions
  , module Language.PureScript.Ide.Logging
  ) where

import           Protolude                           hiding (decodeUtf8,
                                                      encodeUtf8)

import           Control.Lens                        ((^.))
import           Data.Aeson
import qualified Data.Text                           as T
import           Data.Text.Lazy.Encoding             (decodeUtf8, encodeUtf8)
import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Conversions
import           Language.PureScript.Ide.Logging
import           Language.PureScript.Ide.Types

identifierFromIdeDeclaration :: IdeDeclaration -> Text
identifierFromIdeDeclaration d = case d of
  IdeDeclValue v -> v ^. ideValueIdent . identT
  IdeDeclType t -> t ^. ideTypeName . properNameT
  IdeDeclTypeSynonym s -> s ^. ideSynonymName . properNameT
  IdeDeclDataConstructor dtor -> dtor ^. ideDtorName . properNameT
  IdeDeclTypeClass name -> P.runProperName name
  IdeDeclValueOperator op -> op ^. ideValueOpName & P.runOpName
  IdeDeclTypeOperator op -> op ^. ideTypeOpName & P.runOpName
  IdeDeclKind name -> P.runProperName name

discardAnn :: IdeDeclarationAnn -> IdeDeclaration
discardAnn (IdeDeclarationAnn _ d) = d

withEmptyAnn :: IdeDeclaration -> IdeDeclarationAnn
withEmptyAnn = IdeDeclarationAnn emptyAnn

unwrapMatch :: Match a -> a
unwrapMatch (Match (_, ed)) = ed

completionFromMatch :: Match IdeDeclarationAnn -> Completion
completionFromMatch (Match (m, IdeDeclarationAnn ann decl)) =
  Completion {..}
  where
    (complIdentifier, complExpandedType) = case decl of
      IdeDeclValue v -> (v ^. ideValueIdent . identT, v ^. ideValueType & prettyTypeT)
      IdeDeclType t -> (t ^. ideTypeName . properNameT, t ^. ideTypeKind & P.prettyPrintKind & toS )
      IdeDeclTypeSynonym s -> (s ^. ideSynonymName . properNameT, s ^. ideSynonymType & prettyTypeT)
      IdeDeclDataConstructor d -> (d ^. ideDtorName . properNameT, d ^. ideDtorType & prettyTypeT)
      IdeDeclTypeClass name -> (P.runProperName name, "class")
      IdeDeclValueOperator (IdeValueOperator op ref precedence associativity typeP) ->
        (P.runOpName op, maybe (showFixity precedence associativity (valueOperatorAliasT ref) op) prettyTypeT typeP)
      IdeDeclTypeOperator (IdeTypeOperator op ref precedence associativity kind) ->
        (P.runOpName op, maybe (showFixity precedence associativity (typeOperatorAliasT ref) op) (toS . P.prettyPrintKind) kind)
      IdeDeclKind k -> (P.runProperName k, "kind")

    complModule = P.runModuleName m

    complType = maybe complExpandedType prettyTypeT (annTypeAnnotation ann)

    complLocation = annLocation ann

    complDocumentation = Nothing

    showFixity p a r o =
      let asso = case a of
            P.Infix -> "infix"
            P.Infixl -> "infixl"
            P.Infixr -> "infixr"
      in T.unwords [asso, show p, r, "as", P.runOpName o]

valueOperatorAliasT
  :: P.Qualified (Either P.Ident (P.ProperName 'P.ConstructorName)) -> Text
valueOperatorAliasT i =
  P.showQualified (either P.runIdent P.runProperName) i

typeOperatorAliasT
  :: P.Qualified (P.ProperName 'P.TypeName) -> Text
typeOperatorAliasT i =
  P.showQualified P.runProperName i

encodeT :: (ToJSON a) => a -> Text
encodeT = toS . decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . encodeUtf8 . toS

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = unwrapPositioned x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = unwrapPositionedRef x
unwrapPositionedRef x = x
