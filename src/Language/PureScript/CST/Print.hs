module Language.PureScript.CST.Print where

import Prelude

import Data.Text (Text)
import qualified Data.Text as Text
import Language.PureScript.CST.Types

printToken :: Token -> Text
printToken = \case
  TokLeftParen             -> "("
  TokRightParen            -> ")"
  TokLeftBrace             -> "{"
  TokRightBrace            -> "}"
  TokLeftSquare            -> "["
  TokRightSquare           -> "]"
  TokLeftArrow ASCII       -> "<-"
  TokLeftArrow Unicode     -> "←"
  TokRightArrow ASCII      -> "->"
  TokRightArrow Unicode    -> "→"
  TokRightFatArrow ASCII   -> "=>"
  TokRightFatArrow Unicode -> "⇒"
  TokDoubleColon ASCII     -> "::"
  TokDoubleColon Unicode   -> "∷"
  TokForall ASCII          -> "forall"
  TokForall Unicode        -> "∀"
  TokEquals                -> "="
  TokPipe                  -> "|"
  TokTick                  -> "`"
  TokDot                   -> "."
  TokComma                 -> ","
  TokUnderscore            -> "_"
  TokBackslash             -> "\\"
  TokLowerName qual name   -> printQual qual <> name
  TokUpperName qual name   -> printQual qual <> name
  TokOperator qual sym     -> printQual qual <> sym
  TokSymbolName qual sym   -> printQual qual <> "(" <> sym <> ")"
  TokSymbolArr Unicode     -> "(→)"
  TokSymbolArr ASCII       -> "(->)"
  TokHole hole             -> "?" <> hole
  TokChar raw _            -> "'" <> raw <> "'"
  TokString raw _          -> "\"" <> raw <> "\""
  TokRawString raw         -> "\"\"\"" <> raw <> "\"\"\""
  TokInt raw _             -> raw
  TokNumber raw _          -> raw
  TokLayoutStart           -> "{"
  TokLayoutSep             -> ";"
  TokLayoutEnd             -> "}"
  TokEof                   -> "<eof>"

printQual :: [Text] -> Text
printQual = Text.concat . map (<> ".")

printTokens :: [SourceToken] -> Text
printTokens toks = Text.concat (map pp toks)
  where
  pp (SourceToken (TokenAnn _ leading trailing) tok) =
    Text.concat (map ppLc leading)
      <> printToken tok
      <> Text.concat (map ppTc trailing)

  ppLc = \case
    Comment raw -> raw
    Space n -> Text.replicate n " "
    Line LF -> "\n"
    Line CRLF -> "\r\n"

  ppTc = \case
    Comment raw -> raw
    Space n -> Text.replicate n " "
    Line _ -> ""
