-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parsers for types
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Types (
    parseType,
    parsePolyType,
    noWildcards,
    parseTypeAtom
) where

import Control.Applicative
import Control.Monad (when, unless)

import Language.PureScript.Types
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Environment

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

-- TODO: remove these deprecation warnings in 0.8
parseArray :: TokenParser Type
parseArray = do
  _ <- squares $ return tyArray
  featureWasRemoved "Array notation is no longer supported. Use Array instead of []."

parseArrayOf :: TokenParser Type
parseArrayOf = do
  _ <- squares $ TypeApp tyArray <$> parseType
  featureWasRemoved "Array notation is no longer supported. Use Array _ instead of [_]."

parseFunction :: TokenParser Type
parseFunction = parens $ rarrow >> return tyFunction

parseObject :: TokenParser Type
parseObject = braces $ TypeApp tyObject <$> parseRow

parseTypeWildcard :: TokenParser Type
parseTypeWildcard = underscore >> return TypeWildcard

parseTypeVariable :: TokenParser Type
parseTypeVariable = do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected ident
  return $ TypeVar ident

parseTypeConstructor :: TokenParser Type
parseTypeConstructor = TypeConstructor <$> parseQualified properName

parseForAll :: TokenParser Type
parseForAll = mkForAll <$> (P.try (reserved "forall") *> P.many1 (indented *> identifier) <* indented <* dot)
                       <*> parseType

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: TokenParser Type
parseTypeAtom = indented *> P.choice (map P.try
            [ parseArray
            , parseArrayOf
            , parseFunction
            , parseObject
            , parseTypeWildcard
            , parseTypeVariable
            , parseTypeConstructor
            , parseForAll
            , parens parseRow
            , parseConstrainedType
            , parens parsePolyType
            ])

parseConstrainedType :: TokenParser Type
parseConstrainedType = do
  constraints <- parens . commaSep1 $ do
    className <- parseQualified properName
    indented
    ty <- P.many parseTypeAtom
    return (className, ty)
  _ <- rfatArrow
  indented
  ty <- parseType
  return $ ConstrainedType constraints ty

parseAnyType :: TokenParser Type
parseAnyType = P.buildExpressionParser operators (buildPostfixParser postfixTable parseTypeAtom) P.<?> "type"
  where
  operators = [ [ P.Infix (return TypeApp) P.AssocLeft ]
              , [ P.Infix (rarrow >> return function) P.AssocRight ] ]
  postfixTable = [ \t -> KindedType t <$> (P.try (indented *> doubleColon) *> parseKind)
                 ]

-- |
-- Parse a monotype
--
parseType :: TokenParser Type
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: TokenParser Type
parsePolyType = parseAnyType

-- |
-- Parse an atomic type with no wildcards
--
noWildcards :: TokenParser Type -> TokenParser Type
noWildcards p = do
  ty <- p
  when (containsWildcards ty) $ P.unexpected "type wildcard"
  return ty

parseNameAndType :: TokenParser t -> TokenParser (String, t)
parseNameAndType p = (,) <$> (indented *> (lname <|> stringLiteral) <* indented <* doubleColon) <*> p

parseRowEnding :: TokenParser Type
parseRowEnding = P.option REmpty $ indented *> pipe *> indented *> parseType

parseRow :: TokenParser Type
parseRow = (curry rowFromList <$> commaSep (parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
