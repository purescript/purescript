{-# LANGUAGE NamedFieldPuns #-}
module Language.PureScript.CST.Errors
  ( ParserError(..)
  , ParserErrorType(..)
  , prettyPrintError
  , prettyPrintErrorMessage
  ) where

import Prelude

import qualified Data.Text as Text
import Data.Char (isSpace, toUpper)
import Language.PureScript.CST.Layout
import Language.PureScript.CST.Print
import Language.PureScript.CST.Types
import Text.Printf (printf)

data ParserErrorType
  = ErrWildcardInType
  | ErrHoleInType
  | ErrExprInBinder
  | ErrExprInDeclOrBinder
  | ErrExprInDecl
  | ErrBinderInDecl
  | ErrRecordUpdateInCtr
  | ErrRecordPunInUpdate
  | ErrRecordCtrInUpdate
  | ErrTypeInConstraint
  | ErrElseInDecl
  | ErrInstanceNameMismatch
  | ErrUnknownFundep
  | ErrImportInDecl
  | ErrGuardInLetBinder
  | ErrKeywordVar
  | ErrKeywordSymbol
  | ErrQuotedPun
  | ErrToken
  | ErrLineFeedInString
  | ErrAstralCodePointInChar
  | ErrCharEscape
  | ErrNumberOutOfRange
  | ErrLeadingZero
  | ErrExpectedFraction
  | ErrExpectedExponent
  | ErrExpectedHex
  | ErrReservedSymbol
  | ErrCharInGap Char
  | ErrModuleName
  | ErrQualifiedName
  | ErrEmptyDo
  | ErrLexeme (Maybe String) [String]
  | ErrEof
  | ErrCustom String
  deriving (Show, Eq, Ord)

data ParserError = ParserError
  { errRange :: SourceRange
  , errToks :: [SourceToken]
  , errStack :: LayoutStack
  , errType :: ParserErrorType
  } deriving (Show, Eq)

prettyPrintError :: ParserError -> String
prettyPrintError pe@(ParserError { errRange }) =
  prettyPrintErrorMessage pe <> " at " <> errPos
  where
  errPos = case errRange of
    SourceRange (SourcePos line col) _ ->
      "line " <> show line <> ", column " <> show col

prettyPrintErrorMessage :: ParserError -> String
prettyPrintErrorMessage (ParserError {..}) = case errType of
  ErrWildcardInType ->
    "Unexpected wildcard in type; type wildcards are only allowed in value annotations"
  ErrHoleInType ->
    "Unexpected hole in type; type holes are only allowed in value annotations"
  ErrExprInBinder ->
    "Expected pattern, saw expression"
  ErrExprInDeclOrBinder ->
    "Expected declaration or pattern, saw expression"
  ErrExprInDecl ->
    "Expected declaration, saw expression"
  ErrBinderInDecl ->
    "Expected declaration, saw pattern"
  ErrRecordUpdateInCtr ->
    "Expected ':', saw '='"
  ErrRecordPunInUpdate ->
    "Expected record update, saw pun"
  ErrRecordCtrInUpdate ->
    "Expected '=', saw ':'"
  ErrTypeInConstraint ->
    "Expected constraint, saw type"
  ErrElseInDecl ->
    "Expected declaration, saw 'else'"
  ErrInstanceNameMismatch ->
    "All instances in a chain must implement the same type class"
  ErrUnknownFundep ->
    "Unknown type variable in functional dependency"
  ErrImportInDecl ->
    "Expected declaration, saw 'import'"
  ErrGuardInLetBinder ->
    "Unexpected guard in let pattern"
  ErrKeywordVar ->
    "Expected variable, saw keyword"
  ErrKeywordSymbol ->
    "Expected symbol, saw reserved symbol"
  ErrQuotedPun ->
    "Unexpected quoted label in record pun, perhaps due to a missing ':'"
  ErrEof ->
    "Unexpected end of input"
  ErrLexeme (Just (hd : _)) _ | isSpace hd ->
    "Illegal whitespace character " <> displayCodePoint hd
  ErrLexeme (Just a) _ ->
    "Unexpected " <> a
  ErrLineFeedInString ->
    "Unexpected line feed in string literal"
  ErrAstralCodePointInChar ->
    "Illegal astral code point in character literal"
  ErrCharEscape ->
    "Illegal character escape code"
  ErrNumberOutOfRange ->
    "Number literal is out of range"
  ErrLeadingZero ->
    "Unexpected leading zeros"
  ErrExpectedFraction ->
    "Expected fraction"
  ErrExpectedExponent ->
    "Expected exponent"
  ErrExpectedHex ->
    "Expected hex digit"
  ErrReservedSymbol ->
    "Unexpected reserved symbol"
  ErrCharInGap ch ->
    "Unexpected character '" <> [ch] <> "' in gap"
  ErrModuleName ->
    "Invalid module name; underscores and primes are not allowed in module names"
  ErrQualifiedName ->
    "Unexpected qualified name"
  ErrEmptyDo ->
    "Expected do statement"
  ErrLexeme _ _ ->
    basicError
  ErrToken
    | SourceToken _ (TokLeftArrow _) : _ <- errToks ->
        "Unexpected \"<-\" in expression, perhaps due to a missing 'do' or 'ado' keyword"
  ErrToken ->
    basicError
  ErrCustom err ->
    err

  where
  basicError = case errToks of
    tok : _ -> basicTokError (tokValue tok)
    [] -> "Unexpected input"

  basicTokError = \case
    TokLayoutStart -> "Unexpected or mismatched indentation"
    TokLayoutSep   -> "Unexpected or mismatched indentation"
    TokLayoutEnd   -> "Unexpected or mismatched indentation"
    TokEof         -> "Unexpected end of input"
    tok            -> "Unexpected token '" <> Text.unpack (printToken tok) <> "'"

  displayCodePoint :: Char -> String
  displayCodePoint x =
    "U+" <> map toUpper (printf "%0.4x" (fromEnum x))
