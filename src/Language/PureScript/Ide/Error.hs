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

import Data.Aeson (KeyValue(..), ToJSON(..), Value, object)
import Data.Aeson.Types qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import Language.PureScript qualified as P
import Language.PureScript.Errors.JSON (toJSONError)
import Language.PureScript.Ide.Types (ModuleIdent, Completion(..))
import Protolude

data IdeError
    = GeneralError Text
    | NotFound Text
    | ModuleNotFound ModuleIdent
    | ModuleFileNotFound ModuleIdent
    | RebuildError [(FilePath, Text)] P.MultipleErrors
    deriving (Show)

instance ToJSON IdeError where
  toJSON (RebuildError files errs) = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= encodeRebuildErrors files errs
    ]
  toJSON err = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= textError err
    ]

encodeRebuildErrors :: [(FilePath, Text)] -> P.MultipleErrors -> Value
encodeRebuildErrors files = toJSON . map encodeRebuildError . P.runMultipleErrors
  where
    encodeRebuildError err = case err of
      (P.ErrorMessage _
       ((P.HoleInferredType name _ _
         (Just P.TSAfter{tsAfterIdentifiers=idents, tsAfterRecordFields=fields})))) ->
        insertTSCompletions name idents (fromMaybe [] fields) (toJSON (toJSONError False P.Error files err))
      _ ->
        (toJSON . toJSONError False P.Error files) err

    insertTSCompletions name idents fields (Aeson.Object value) =
      Aeson.Object
        (KM.insert "pursIde"
         (object [ "name" .= name
                 , "completions" .= ordNub (map identCompletion idents ++ map fieldCompletion fields)
                 ]) value)
    insertTSCompletions _ _ _ v = v

    identCompletion (P.Qualified mn i, ty) =
      Completion     
        { complModule = maybe "" P.runModuleName $ P.toMaybeModuleName mn
        , complIdentifier = i
        , complType = prettyPrintTypeSingleLine ty
        , complExpandedType = prettyPrintTypeSingleLine ty
        , complLocation = Nothing
        , complDocumentation = Nothing
        , complExportedFrom = toList $ P.toMaybeModuleName mn
        , complDeclarationType = Nothing
        }
    fieldCompletion (label, ty) =
      Completion 
        { complModule = ""
        , complIdentifier = "_." <> P.prettyPrintLabel label
        , complType = prettyPrintTypeSingleLine ty
        , complExpandedType = prettyPrintTypeSingleLine ty
        , complLocation = Nothing
        , complDocumentation = Nothing
        , complExportedFrom = []
        , complDeclarationType = Nothing
        }

textError :: IdeError -> Text
textError (GeneralError msg)          = msg
textError (NotFound ident)            = "Symbol '" <> ident <> "' not found."
textError (ModuleNotFound ident)      = "Module '" <> ident <> "' not found."
textError (ModuleFileNotFound ident)  = "Extern file for module " <> ident <> " could not be found"
textError (RebuildError _ err)        = show err

prettyPrintTypeSingleLine :: P.Type a -> Text
prettyPrintTypeSingleLine = T.unwords . map T.strip . T.lines . T.pack . P.prettyPrintTypeWithUnicode maxBound
