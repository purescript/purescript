{-# LANGUAGE OverloadedStrings #-}

module Language.PureScript.DevTools.JSONPrint where

import Data.Text.Encoding
import Data.Aeson hiding (Value)
import Data.Text as T
import Data.ByteString.Lazy
import Language.PureScript.DevTools.ErrorTypes


jsonPrintErrorStack :: Bool -> ErrorStack -> String
jsonPrintErrorStack _ = T.unpack . decodeUtf8 . toStrict . encode
