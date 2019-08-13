{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module TestCst where

import Prelude

import Control.Monad (when)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Test.Tasty.QuickCheck
import Text.Read (readMaybe)
import Language.PureScript.CST.Errors as CST
import Language.PureScript.CST.Lexer as CST
import Language.PureScript.CST.Print as CST
import Language.PureScript.CST.Types
import System.FilePath (takeBaseName, replaceExtension)

main :: IO TestTree
main = do
  lytTests <- layoutTests
  pure $ testGroup "cst"
    [ lytTests
    , litTests
    ]

layoutTests :: IO TestTree
layoutTests = do
  pursFiles <- findByExtension [".purs"] "./tests/purs/layout"
  return $ testGroup "Layout golden tests" $ do
    file <- pursFiles
    pure $ goldenVsString
      (takeBaseName file)
      (replaceExtension file ".out")
      (BS.fromStrict . Text.encodeUtf8 <$> runLexer file)
  where
  runLexer file = do
    src <- Text.readFile file
    case sequence $ CST.lex src of
      Left (_, err) ->
        pure $ Text.pack $ CST.prettyPrintError err
      Right toks -> do
        pure $ CST.printTokens toks

litTests :: TestTree
litTests = testGroup "Literals"
  [ testProperty "Integer" $
      checkTok checkReadNum (\case TokInt _ a -> Just a; _ -> Nothing) . unInt
  , testProperty "Hex" $
      checkTok checkReadNum (\case TokInt _ a -> Just a; _ -> Nothing) . unHex
  , testProperty "Number" $
      checkTok checkReadNum (\case TokNumber _ a -> Just a; _ -> Nothing) . unFloat
  , testProperty "Exponent" $
      checkTok checkReadNum (\case TokNumber _ a -> Just a; _ -> Nothing) . unExponent

  , testProperty "Integer (round trip)" $ roundTripTok . unInt
  , testProperty "Hex (round trip)" $ roundTripTok . unHex
  , testProperty "Number (round trip)" $ roundTripTok . unFloat
  , testProperty "Exponent (round trip)" $ roundTripTok . unExponent
  , testProperty "Char (round trip)" $ roundTripTok . unChar
  , testProperty "String (round trip)" $ roundTripTok . unString
  , testProperty "Raw String (round trip)" $ roundTripTok . unRawString
  ]

readTok' :: String -> Text -> Gen SourceToken
readTok' failMsg t = case CST.lex t of
  Right tok : _ ->
    pure tok
  Left (_, err) : _ ->
    fail $ failMsg <> ": " <> CST.prettyPrintError err
  [] ->
    fail "Empty token stream"

readTok :: Text -> Gen SourceToken
readTok = readTok' "Failed to parse"

checkTok
  :: (Text -> a -> Gen Bool)
  -> (Token -> Maybe a)
  -> Text
  -> Gen Bool
checkTok p f t = do
  SourceToken _ tok <- readTok t
  case f tok of
    Just a  -> p t a
    Nothing -> fail $ "Failed to lex correctly: " <> show tok

roundTripTok :: Text -> Gen Bool
roundTripTok t = do
  tok <- readTok t
  let t' = CST.printTokens [tok]
  tok' <- readTok' "Failed to re-parse" t'
  pure $ tok == tok'

checkReadNum :: (Eq a, Read a) => Text -> a -> Gen Bool
checkReadNum t a = do
  let
    chs = case Text.unpack $ Text.replace ".e" ".0e" $ Text.replace "_" "" t of
      chs' | last chs' == '.' -> chs' <> "0"
      chs' -> chs'
  case (== a) <$> readMaybe chs of
    Just a' -> pure a'
    Nothing -> fail "Failed to `read`"

newtype PSSourceInt = PSSourceInt { unInt :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceInt where
  arbitrary = resize 16 genInt

newtype PSSourceFloat = PSSourceFloat { unFloat :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceFloat where
  arbitrary = resize 16 genFloat

newtype PSSourceExponent = PSSourceExponent { unExponent :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceExponent where
  arbitrary = PSSourceExponent <$> do
    floatPart <- unFloat <$> resize 5 genFloat
    signPart <- fromMaybe "" <$> elements [ Just "+", Just "-", Nothing ]
    expPart <- unInt <$> resize 1 genInt
    pure $ floatPart <> "e" <> signPart <> expPart

newtype PSSourceHex = PSSourceHex { unHex :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceHex where
  arbitrary = resize 16 genHex

newtype PSSourceChar = PSSourceChar { unChar :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceChar where
  arbitrary = genChar

newtype PSSourceString = PSSourceString { unString :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceString where
  arbitrary = resize 256 genString

newtype PSSourceRawString = PSSourceRawString { unRawString :: Text }
  deriving (Show, Eq)

instance Arbitrary PSSourceRawString where
  arbitrary = resize 256 genRawString

genInt :: Gen PSSourceInt
genInt = PSSourceInt . Text.pack <$> do
  (:) <$> nonZeroChar
      <*> listOf numChar

genFloat :: Gen PSSourceFloat
genFloat = PSSourceFloat <$> do
  intPart <- unInt <$> genInt
  floatPart <- Text.pack <$> listOf1 numChar
  pure $ intPart <> "." <> floatPart

genHex :: Gen PSSourceHex
genHex = PSSourceHex <$> do
  nums <- listOf1 hexDigit
  pure $ "0x" <> Text.pack nums

genChar :: Gen PSSourceChar
genChar = PSSourceChar <$> do
  ch <- resize 0xFFFF arbitrarySizedNatural >>= (genStringChar '\'' . toEnum)
  pure $ "'" <> ch <> "'"

genString :: Gen PSSourceString
genString = PSSourceString <$> do
  chs <- listOf $ arbitraryUnicodeChar >>= genStringChar '"'
  pure $ "\"" <> Text.concat chs <> "\""

genStringChar :: Char -> Char -> Gen Text
genStringChar delimiter ch = frequency
  [ (1, genCharEscape)
  , (10, if ch `elem` [delimiter, '\n', '\r', '\\']
           then discard
           else pure $ Text.singleton ch
    )
  ]

genRawString :: Gen PSSourceRawString
genRawString = PSSourceRawString <$> do
  chs <- listOf $ arbitraryUnicodeChar
  let
    k1 acc qs cs = do
      let (cs', q) = span (/= '"') cs
      k2 (acc <> cs') qs q
    k2 acc qs [] = acc <> qs
    k2 acc qs cs = do
      let (q, cs') = span (== '"') cs
      k1 (acc <> take 2 q) (qs <> drop 2 q) cs'
    chs' = k1 [] [] chs
  when (all (== '"') chs') discard
  pure $ "\"\"\"" <> Text.pack chs' <> "\"\"\""

genCharEscape :: Gen Text
genCharEscape = oneof
  [ pure "\\t"
  , pure "\\r"
  , pure "\\n"
  , pure "\\\""
  , pure "\\'"
  , pure "\\\\"
  , do
      chs <- resize 4 $ listOf1 hexDigit
      pure $ "\\x" <> Text.pack chs
  ]

numChar :: Gen Char
numChar = elements "0123456789_"

nonZeroChar :: Gen Char
nonZeroChar = elements "123456789"

hexDigit :: Gen Char
hexDigit = elements $ ['a'..'f'] <> ['A'..'F'] <> ['0'..'9']
