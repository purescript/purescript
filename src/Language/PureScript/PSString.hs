{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.PSString
  ( PSString
  , toUTF16CodeUnits
  , decodeString
  , decodeStringEither
  , decodeStringWithReplacement
  , prettyPrintString
  , prettyPrintStringJS
  , mkString
  ) where

import Prelude.Compat
import Control.Exception (try, evaluate)
import Control.Applicative ((<|>))
import Data.Char (chr)
import Data.Bits (shiftR)
import Data.List (unfoldr)
import Data.Monoid ((<>))
import Data.Scientific (toBoundedInteger)
import Data.String (IsString(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf16BE)
import Data.Text.Encoding.Error (UnicodeException)
import qualified Data.Vector as V
import Data.Word (Word16, Word8)
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A

-- |
-- Strings in PureScript are sequences of UTF-16 code units, which do not
-- necessarily represent UTF-16 encoded text. For example, it is permissible
-- for a string to contain *lone surrogates,* i.e. characters in the range
-- U+D800 to U+DFFF which do not appear as a part of a surrogate pair.
--
-- The Show instance for PSString produces a string literal which would
-- represent the same data were it inserted into a PureScript source file.
--
-- Because JSON parsers vary wildly in terms of how they deal with lone
-- surrogates in JSON strings, the ToJSON instance for PSString produces JSON
-- strings where that would be safe (i.e. when there are no lone surrogates),
-- and arrays of UTF-16 code units (integers) otherwise.
--
newtype PSString = PSString { toUTF16CodeUnits :: [Word16] }
  deriving (Eq, Ord, Monoid)

instance Show PSString where
  show = show . codePoints

-- |
-- Decode a PSString to a String, representing any lone surrogates as the
-- reserved code point with that index. Warning: if there are any lone
-- surrogates, converting the result to Text via Data.Text.pack will result in
-- loss of information as those lone surrogates will be replaced with U+FFFD
-- REPLACEMENT CHARACTER. Because this function requires care to use correctly,
-- we do not export it.
--
codePoints :: PSString -> String
codePoints = map (either (chr . fromIntegral) id) . decodeStringEither

-- |
-- Decode a PSString as UTF-16 text. Lone surrogates will be replaced with
-- U+FFFD REPLACEMENT CHARACTER
--
decodeStringWithReplacement :: PSString -> String
decodeStringWithReplacement = map (either (const '\xFFFD') id) . decodeStringEither

-- |
-- Decode a PSString as UTF-16. Lone surrogates in the input are represented in
-- the output with the Left constructor; characters which were successfully
-- decoded are represented with the Right constructor.
--
decodeStringEither :: PSString -> [Either Word16 Char]
decodeStringEither = unfoldr decode . toUTF16CodeUnits
  where
  decode :: [Word16] -> Maybe (Either Word16 Char, [Word16])
  decode (h:l:rest) | isLead h && isTrail l = Just (Right (unsurrogate h l), rest)
  decode (c:rest) | isSurrogate c = Just (Left c, rest)
  decode (c:rest) = Just (Right (toChar c), rest)
  decode [] = Nothing

  unsurrogate :: Word16 -> Word16 -> Char
  unsurrogate h l = toEnum ((toInt h - 0xD800) * 0x400 + (toInt l - 0xDC00) + 0x10000)

-- |
-- Pretty print a PSString, using Haskell/PureScript escape sequences.
-- This is identical to the Show instance except that we get a Text out instead
-- of a String.
--
prettyPrintString :: PSString -> Text
prettyPrintString = T.pack . show

-- |
-- Attempt to decode a PSString as UTF-16 text. This will fail (returning
-- Nothing) if the argument contains lone surrogates.
--
decodeString :: PSString -> Maybe Text
decodeString = hush . decodeEither . BS.pack . concatMap unpair . toUTF16CodeUnits
  where
  unpair w = [highByte w, lowByte w]

  lowByte :: Word16 -> Word8
  lowByte = fromIntegral

  highByte :: Word16 -> Word8
  highByte = fromIntegral . (`shiftR` 8)

  -- Based on a similar function from Data.Text.Encoding for utf8. This is a
  -- safe usage of unsafePerformIO because there are no side effects after
  -- handling any thrown UnicodeExceptions.
  decodeEither :: ByteString -> Either UnicodeException Text
  decodeEither = unsafePerformIO . try . evaluate . decodeUtf16BE

  hush = either (const Nothing) Just

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
  toJSON str =
    case decodeString str of
      Just t -> A.toJSON t
      Nothing -> A.toJSON (toUTF16CodeUnits str)

instance A.FromJSON PSString where
  parseJSON a = jsonString <|> arrayOfCodeUnits
    where
    jsonString = fromString <$> A.parseJSON a

    arrayOfCodeUnits = PSString <$> parseArrayOfCodeUnits a

    parseArrayOfCodeUnits :: A.Value -> A.Parser [Word16]
    parseArrayOfCodeUnits = A.withArray "array of UTF-16 code units" (traverse parseCodeUnit . V.toList)

    parseCodeUnit :: A.Value -> A.Parser Word16
    parseCodeUnit b = A.withScientific "two-byte non-negative integer" (maybe (A.typeMismatch "" b) return . toBoundedInteger) b

-- |
-- Pretty print a PSString, using JavaScript escape sequences. Intended for
-- use in compiled JS output.
--
prettyPrintStringJS :: PSString -> Text
prettyPrintStringJS s = "\"" <> foldMap encodeChar (toUTF16CodeUnits s) <> "\""
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

isSurrogate :: Word16 -> Bool
isSurrogate c = isLead c || isTrail c

toChar :: Word16 -> Char
toChar = toEnum . fromIntegral

toWord :: Int -> Word16
toWord = fromIntegral

toInt :: Word16 -> Int
toInt = fromIntegral

mkString :: Text -> PSString
mkString = fromString . T.unpack
