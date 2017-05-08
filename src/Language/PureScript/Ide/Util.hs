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
  , properNameT
  , identT
  , opNameT
  , ideReadFile
  , ideReadTextFile
  , module Language.PureScript.Ide.Logging
  ) where

import           Protolude                           hiding (decodeUtf8,
                                                      encodeUtf8)

import           Control.Lens                        hiding ((&), op)
import           Data.Aeson
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import           Data.Text.Lazy.Encoding             as TLE
import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Error       (prettyPrintTypeSingleLine, IdeError(..))
import           Language.PureScript.Ide.Logging
import           Language.PureScript.Ide.Types
import           System.IO.UTF8                      (readUTF8FileT)

identifierFromIdeDeclaration :: IdeDeclaration -> Text
identifierFromIdeDeclaration d = case d of
  IdeDeclValue v -> v ^. ideValueIdent . identT
  IdeDeclType t -> t ^. ideTypeName . properNameT
  IdeDeclTypeSynonym s -> s ^. ideSynonymName . properNameT
  IdeDeclDataConstructor dtor -> dtor ^. ideDtorName . properNameT
  IdeDeclTypeClass tc -> tc ^. ideTCName . properNameT
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
      IdeDeclValue v -> (v ^. ideValueIdent . identT, v ^. ideValueType & prettyPrintTypeSingleLine)
      IdeDeclType t -> (t ^. ideTypeName . properNameT, t ^. ideTypeKind & P.prettyPrintKind)
      IdeDeclTypeSynonym s -> (s ^. ideSynonymName . properNameT, s ^. ideSynonymType & prettyPrintTypeSingleLine)
      IdeDeclDataConstructor d -> (d ^. ideDtorName . properNameT, d ^. ideDtorType & prettyPrintTypeSingleLine)
      IdeDeclTypeClass d -> (d ^. ideTCName . properNameT, d ^. ideTCKind & P.prettyPrintKind)
      IdeDeclValueOperator (IdeValueOperator op ref precedence associativity typeP) ->
        (P.runOpName op, maybe (showFixity precedence associativity (valueOperatorAliasT ref) op) prettyPrintTypeSingleLine typeP)
      IdeDeclTypeOperator (IdeTypeOperator op ref precedence associativity kind) ->
        (P.runOpName op, maybe (showFixity precedence associativity (typeOperatorAliasT ref) op) P.prettyPrintKind kind)
      IdeDeclKind k -> (P.runProperName k, "kind")

    complModule = P.runModuleName m

    complType = maybe complExpandedType prettyPrintTypeSingleLine (_annTypeAnnotation ann)

    complLocation = _annLocation ann

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
encodeT = TL.toStrict . TLE.decodeUtf8 . encode

decodeT :: (FromJSON a) => Text -> Maybe a
decodeT = decode . TLE.encodeUtf8 . TL.fromStrict

unwrapPositioned :: P.Declaration -> P.Declaration
unwrapPositioned (P.PositionedDeclaration _ _ x) = unwrapPositioned x
unwrapPositioned x = x

unwrapPositionedRef :: P.DeclarationRef -> P.DeclarationRef
unwrapPositionedRef (P.PositionedDeclarationRef _ _ x) = unwrapPositionedRef x
unwrapPositionedRef x = x

properNameT :: Iso' (P.ProperName a) Text
properNameT = iso P.runProperName P.ProperName

identT :: Iso' P.Ident Text
identT = iso P.runIdent P.Ident

opNameT :: Iso' (P.OpName a) Text
opNameT = iso P.runOpName P.OpName

ideReadFile'
  :: (MonadIO m, MonadError IdeError m)
  => (FilePath -> IO Text)
  -> FilePath
  -> m Text
ideReadFile' fileReader fp = do
  contents :: Either IOException Text <- liftIO (try (fileReader fp))
  either
    (\_ -> throwError (GeneralError ("Couldn't find file at: " <> T.pack fp)))
    pure
    contents

ideReadFile :: (MonadIO m, MonadError IdeError m) => FilePath -> m Text
ideReadFile = ideReadFile' readUTF8FileT

-- | This function is to be used over @ideReadFile@ when the result is not just
-- passed on to the PureScript parser, but also needs to be treated as lines of
-- Text. Because @ideReadFile@ reads the file as ByteString line endings get
-- mangled on Windows otherwise. This is irrelevant for parsing, because the
-- Lexer strips the additional @\r@s as whitespace.
ideReadTextFile :: (MonadIO m, MonadError IdeError m) => FilePath -> m Text
ideReadTextFile fp = T.replace "\r\n" "\n" <$> ideReadFile fp
