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
       ( PscIdeError(..)
       ) where

import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Ide.Types   (ModuleIdent)
import           Protolude
import qualified Text.Parsec.Error               as P

data PscIdeError
    = GeneralError Text
    | NotFound Text
    | ModuleNotFound ModuleIdent
    | ModuleFileNotFound ModuleIdent
    | ParseError P.ParseError Text
    | RebuildError [(JSONError, Maybe Text)]

instance ToJSON PscIdeError where
  toJSON (RebuildError errs) = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= map addHint errs
    ]
  toJSON err = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= textError err
    ]

addHint :: (ToJSON a, ToJSON b) => (a, b) -> Value
addHint (err, d) = addKey ("psc-ide-hint", toJSON d) (toJSON err )

addKey :: (Text, Value) -> Value -> Value
addKey (k, v) (Object hashMap) = Object (HM.insert k v hashMap)
addKey _ _ = panic "Not an object"

textError :: PscIdeError -> Text
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
