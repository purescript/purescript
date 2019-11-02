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
                                                      encodeUtf8, to)

import           Data.Aeson
import qualified Data.Text.Lazy                      as TL
import           Data.Text.Lazy.Encoding             as TLE
import qualified Language.PureScript                 as P
import           Language.PureScript.Ide.Error       (IdeError(..))
import           Language.PureScript.Ide.Logging
import           Language.PureScript.Ide.Types
import           Lens.Micro.Platform                 hiding ((&))
import           System.IO.UTF8                      (readUTF8FileT)
import           System.Directory                    (makeAbsolute)

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
  IdeDeclModule name -> P.runModuleName name

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
  IdeDeclModule _ -> IdeNSModule

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

properNameT :: Getting r (P.ProperName a) Text
properNameT = to P.runProperName

identT :: Getting r P.Ident Text
identT = to P.runIdent

opNameT :: Getting r (P.OpName a) Text
opNameT = to P.runOpName

ideReadFile'
  :: (MonadIO m, MonadError IdeError m)
  => (FilePath -> IO Text)
  -> FilePath
  -> m (FilePath, Text)
ideReadFile' fileReader fp = do
  absPath <- liftIO (try (makeAbsolute fp)) >>= \case
    Left (err :: IOException) ->
      throwError
        (GeneralError
          ("Couldn't resolve path for: " <> show fp <> ", Error: " <> show err))
    Right absPath -> pure absPath
  contents <- liftIO (try (fileReader absPath)) >>= \case
    Left (err :: IOException) ->
      throwError
        (GeneralError
          ("Couldn't find file at: " <> show absPath <> ", Error: " <> show err))
    Right contents ->
      pure contents
  pure (absPath, contents)

ideReadFile :: (MonadIO m, MonadError IdeError m) => FilePath -> m (FilePath, Text)
ideReadFile = ideReadFile' readUTF8FileT
