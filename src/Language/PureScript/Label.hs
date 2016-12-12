{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.PureScript.Label (Label(..), renderPSLabel) where

import Prelude.Compat hiding (lex)
import Data.Monoid ()
import Data.String (IsString(..))
import Data.Text (Text)
import Text.Parsec.Combinator (eof)
import qualified Data.Aeson as A

import Language.PureScript.Parser.Common (runTokenParser)
import Language.PureScript.Parser.Lexer (TokenParser, lname, lex)
import Language.PureScript.PSString (PSString, codePoints, renderPSString)

-- |
-- Labels are used as record keys and row entry names. Labels newtype PSString
-- because records are indexable by PureScript strings at runtime.
--
newtype Label = Label PSString
  deriving (Show, Eq, Ord, IsString, Monoid, A.ToJSON, A.FromJSON)

renderPSLabel :: Label -> Text
renderPSLabel (Label s) =
  let quoted = const $ renderPSString s in
  either quoted (\t -> either quoted (const t) $ runParser lname t) $ codePoints s

runParser :: TokenParser a -> Text -> Either String a
runParser p s = either (Left . show) Right $ do
  ts <- lex "" s
  runTokenParser "" (p <* eof) ts
