{-# LANGUAGE TemplateHaskell #-}
module Language.PureScript.Docs.Css where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.FileEmbed (embedFile)

-- |
-- An embedded copy of normalize.css as a UTF-8 encoded ByteString; this should
-- be included before pursuit.css in any HTML page using pursuit.css.
--
normalizeCss :: ByteString
normalizeCss = $(embedFile "app/static/normalize.css")

-- |
-- Like 'normalizeCss', but as a 'Text'.
normalizeCssT :: Text
normalizeCssT = decodeUtf8 normalizeCss

-- |
-- CSS for use with generated HTML docs, as a UTF-8 encoded ByteString.
--
pursuitCss :: ByteString
pursuitCss = $(embedFile "app/static/pursuit.css")

-- |
-- Like 'pursuitCss', but as a 'Text'.
--
pursuitCssT :: Text
pursuitCssT = decodeUtf8 pursuitCss
