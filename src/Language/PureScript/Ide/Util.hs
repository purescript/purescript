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
  , namespaceForDeclaration
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
import           Language.PureScript.Ide.Error       (IdeError(..))
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

namespaceForDeclaration :: IdeDeclaration -> IdeNamespace
namespaceForDeclaration d = case d of
  IdeDeclValue _ -> IdeNSValue
  IdeDeclType _ -> IdeNSType
  IdeDeclTypeSynonym _ -> IdeNSType
  IdeDeclDataConstructor _ -> IdeNSValue
  IdeDeclTypeClass _ -> IdeNSType
  IdeDeclValueOperator _ -> IdeNSValue
  IdeDeclTypeOperator _ -> IdeNSType
  IdeDeclKind _ -> IdeNSKind

discardAnn :: IdeDeclarationAnn -> IdeDeclaration
discardAnn (IdeDeclarationAnn _ d) = d

withEmptyAnn :: IdeDeclaration -> IdeDeclarationAnn
withEmptyAnn = IdeDeclarationAnn emptyAnn

unwrapMatch :: Match a -> a
unwrapMatch (Match (_, ed)) = ed

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
