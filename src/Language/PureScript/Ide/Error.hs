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

{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.Ide.Error
       (ErrorMsg, PscIdeError(..), textError)
       where

import           Prelude.Compat
import           Data.Aeson
import           Data.Monoid
import           Data.Text                     (Text, pack)
import           Language.PureScript.Errors.JSON
import           Language.PureScript.Ide.Types (ModuleIdent)
import qualified Text.Parsec.Error             as P

type ErrorMsg = String

data PscIdeError
    = GeneralError ErrorMsg
    | NotFound Text
    | ModuleNotFound ModuleIdent
    | ModuleFileNotFound ModuleIdent
    | ParseError P.ParseError ErrorMsg
    | RebuildError [JSONError]

instance ToJSON PscIdeError where
  toJSON (RebuildError errs) = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= errs
    ]
  toJSON err = object
    [ "resultType" .= ("error" :: Text)
    , "result" .= textError err
    ]

textError :: PscIdeError -> Text
textError (GeneralError msg)          = pack msg
textError (NotFound ident)            = "Symbol '" <> ident <> "' not found."
textError (ModuleNotFound ident)      = "Module '" <> ident <> "' not found."
textError (ModuleFileNotFound ident)  = "Extern file for module " <> ident <>" could not be found"
textError (ParseError parseError msg) = let escape = show
                                            -- escape newlines and other special
                                            -- chars so we can send the error
                                            -- over the socket as a single line
                                        in pack $ msg <> ": " <> show (escape parseError)
textError (RebuildError err)          = pack (show err)
