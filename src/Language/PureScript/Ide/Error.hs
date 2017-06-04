-----------------------------------------------------------------------------
--
-- Module      : Language.PureScript.Ide.Error
-- Description : Error types for psc-ide
-- Copyright   : Christoph Hegemann 2016
-- License     : MIT (http://opensource.org/licenses/MIT)
--
-- Maintainer  : Christoph Hegemann <christoph.hegemann1337@gmail.com>
-- Stability   : experimental
--
-- |
-- Error types for psc-ide
-----------------------------------------------------------------------------

module Language.PureScript.Ide.Error
       ( IdeError(..)
       , prettyPrintTypeSingleLine
       ) where

import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import qualified Language.PureScript as P
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Ide.Types   (ModuleIdent, Completion(..))
import           Protolude
import qualified Text.Parsec.Error               as Parsec

data IdeError
    = GeneralError Text
    | NotFound Text
    | ModuleNotFound ModuleIdent
    | ModuleFileNotFound ModuleIdent
    | ParseError Parsec.ParseError Text
    | RebuildError P.MultipleErrors
    deriving (Show)

instance ToJSON IdeError where
  toJSON (RebuildError errs) = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= encodeRebuildErrors errs
    ]
  toJSON err = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= textError err
    ]

encodeRebuildErrors :: P.MultipleErrors -> Value
encodeRebuildErrors = toJSON . map encodeRebuildError . P.runMultipleErrors
  where
    encodeRebuildError err = case err of
      (P.ErrorMessage _
       ((P.HoleInferredType name _ _
         (P.TSAfter{tsAfterIdentifiers=idents, tsAfterRecordFields=fields})))) ->
        insertTSCompletions name idents (fromMaybe [] fields) (toJSON (toJSONError False P.Error err))
      _ ->
        (toJSON . toJSONError False P.Error) err

    insertTSCompletions name idents fields (Aeson.Object value) =
      Aeson.Object
        (HM.insert "pursIde"
         (object [ "name" .= name
                 , "completions" .= (ordNub (map identCompletion idents ++ map fieldCompletion fields))
                 ]) value)
    insertTSCompletions _ _ _ v = v

    identCompletion (P.Qualified mn i, ty) =
      Completion (maybe "" P.runModuleName mn) i (prettyPrintTypeSingleLine ty) (prettyPrintTypeSingleLine ty) Nothing Nothing (maybe [] (\x -> [x]) mn)
    fieldCompletion (label, ty) =
      Completion "" ("_." <> P.prettyPrintLabel label) (prettyPrintTypeSingleLine ty) (prettyPrintTypeSingleLine ty) Nothing Nothing []

textError :: IdeError -> Text
textError (GeneralError msg)          = msg
textError (NotFound ident)            = "Symbol '" <> ident <> "' not found."
textError (ModuleNotFound ident)      = "Module '" <> ident <> "' not found."
textError (ModuleFileNotFound ident)  = "Extern file for module " <> ident <>" could not be found"
textError (ParseError parseError msg) = let escape = show
                                            -- escape newlines and other special
                                            -- chars so we can send the error
                                            -- over the socket as a single line
                                        in msg <> ": " <> escape parseError
textError (RebuildError err)          = show err

prettyPrintTypeSingleLine :: P.Type -> Text
prettyPrintTypeSingleLine = T.unwords . map T.strip . T.lines . T.pack . P.prettyPrintTypeWithUnicode
