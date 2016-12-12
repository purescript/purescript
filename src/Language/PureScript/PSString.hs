{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.PSString (PSString, mkString, renderPSString, renderJSON, toUTF16CodeUnits, codePoints, containsLoneSurrogates) where

import Prelude.Compat
import Numeric (showHex)
import Data.List (unfoldr)
import Data.Monoid ((<>))
import Data.Scientific (toBoundedInteger)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Word (Word16)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Vector as V
import qualified Data.Text as T

-- |
-- Strings in PureScript are sequences of UTF-16 code units, which do not
-- necessarily represent UTF-16 encoded text. For example, it is permissible
-- for a string to contain *lone surrogates,* i.e. characters in the range
-- U+D800 to U+DFFF which do not appear as a part of a surrogate pair.
--
--
newtype PSString = PSString [Word16]
  deriving (Eq, Ord, Monoid)

instance Show PSString where
  show = either show show . codePoints

renderPSString :: PSString -> Text
renderPSString = T.pack . either show show . codePoints

-- NOTE: lone surrogates in the given PSString are represented in the resulting
--       String as the reserved code point with that index
codePoints :: PSString -> Either String Text
codePoints s = (if containsLoneSurrogates s then Left . unfoldr decode else Right . T.unfoldr decode) $ toUTF16CodeUnits s
  where
  decode :: [Word16] -> Maybe (Char, [Word16])
  decode (h:l:rest) | isLead h && isTrail l = Just (unsurrogate h l, rest)
  decode (c:rest) = Just (toChar c, rest)
  decode [] = Nothing

  unsurrogate :: Word16 -> Word16 -> Char
  unsurrogate h l = toEnum ((toInt h - 0xD800) * 0x400 + (toInt l - 0xDC00) + 0x10000)

containsLoneSurrogates :: PSString -> Bool
containsLoneSurrogates = or . unfoldr headIsLoneSurrogate . toUTF16CodeUnits
  where
  headIsLoneSurrogate :: [Word16] -> Maybe (Bool, [Word16])
  headIsLoneSurrogate (h:l:rest) | isLead h && isTrail l = Just (False, rest)
  headIsLoneSurrogate (c:rest) = Just (isLead c || isTrail c, rest)
  headIsLoneSurrogate [] = Nothing

instance IsString PSString where
  fromString a = PSString $ concatMap encodeUTF16 a
    where
    surrogates :: Char -> (Word16, Word16)
    surrogates c = (toWord (h + 0xD800), toWord (l + 0xDC00))
      where (h, l) = divMod (fromEnum c - 0x10000) 0x400

    encodeUTF16 :: Char -> [Word16]
    encodeUTF16 c | fromEnum c > 0xFFFF = [high, low]
      where (high, low) = surrogates c
    encodeUTF16 c = [toWord $ fromEnum c]

instance A.ToJSON PSString where
  toJSON = A.toJSON . toUTF16CodeUnits

instance A.FromJSON PSString where
  parseJSON a = PSString <$> parseArrayOfCodeUnits a
    where
    parseArrayOfCodeUnits :: A.Value -> A.Parser [Word16]
    parseArrayOfCodeUnits = A.withArray "array of UTF-16 code units" $ \b -> sequence (parseCodeUnit <$> V.toList b)
    parseCodeUnit :: A.Value -> A.Parser Word16
    parseCodeUnit b = A.withScientific "two-byte non-negative integer" (maybe (A.typeMismatch "" b) return . toBoundedInteger) b

renderJSON :: PSString -> Text
renderJSON s = "\"" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\""
  where
  encodeChar :: Word16 -> Text
  encodeChar c | c > 0xFF = "\\u" <> hex 4 c
  encodeChar c | c > 0x7E || c < 0x20 = "\\x" <> hex 2 c
  encodeChar c | toChar c == '\b' = "\\b"
  encodeChar c | toChar c == '\t' = "\\t"
  encodeChar c | toChar c == '\n' = "\\n"
  encodeChar c | toChar c == '\v' = "\\v"
  encodeChar c | toChar c == '\f' = "\\f"
  encodeChar c | toChar c == '\r' = "\\r"
  encodeChar c | toChar c == '"'  = "\\\""
  encodeChar c | toChar c == '\\' = "\\\\"
  encodeChar c = T.singleton $ toChar c

  hex :: (Enum a) => Int -> a -> Text
  hex width c =
    let hs = showHex (fromEnum c) "" in
    T.pack (replicate (width - length hs) '0' <> hs)

isLead :: Word16 -> Bool
isLead h = h >= 0xD800 && h <= 0xDBFF

isTrail :: Word16 -> Bool
isTrail l = l >= 0xDC00 && l <= 0xDFFF

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

toWord :: Int -> Word16
toWord = fromIntegral

toInt :: Word16 -> Int
toInt = fromIntegral

mkString :: Text -> PSString
mkString = fromString . T.unpack

toUTF16CodeUnits :: PSString -> [Word16]
toUTF16CodeUnits (PSString s) = s
