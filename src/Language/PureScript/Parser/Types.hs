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

parseArray :: TokenParser u Type
parseArray = squares $ return tyArray

parseArrayOf :: TokenParser u Type
parseArrayOf = squares $ TypeApp tyArray <$> parseType

parseFunction :: TokenParser u Type
parseFunction = parens $ P.try rarrow >> return tyFunction

parseObject :: TokenParser u Type
parseObject = braces $ TypeApp tyObject <$> parseRow

parseTypeWildcard :: TokenParser u Type
parseTypeWildcard = reserved "_" >> return TypeWildcard

parseTypeVariable :: TokenParser u Type
parseTypeVariable = do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected ident
  return $ TypeVar ident
  where
  reservedTypeNames :: [String]
  reservedTypeNames = [ "forall", "where" ]

parseTypeConstructor :: TokenParser u Type
parseTypeConstructor = TypeConstructor <$> parseQualified properName

parseForAll :: TokenParser u Type
parseForAll = mkForAll <$> (P.try (reserved "forall") *> P.many1 identifier <* dot)
                       <*> parseConstrainedType

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: TokenParser u Type
parseTypeAtom = P.choice $ map P.try
            [ parseArray
            , parseArrayOf
            , parseFunction
            , parseObject
            , parseTypeWildcard
            , parseTypeVariable
            , parseTypeConstructor
            , parseForAll
            , parens parseRow
            , parens parsePolyType ]

parseConstrainedType :: TokenParser u Type
parseConstrainedType = do
  constraints <- P.optionMaybe . P.try $ do
    constraints <- parens . commaSep1 $ do
      className <- parseQualified properName
      ty <- P.many parseTypeAtom
      return (className, ty)
    _ <- rfatArrow
    return constraints
  ty <- parseType
  return $ maybe ty (flip ConstrainedType ty) constraints

parseAnyType :: TokenParser u Type
parseAnyType = P.buildExpressionParser operators (buildPostfixParser postfixTable parseTypeAtom) P.<?> "type"
  where
  operators = [ [ P.Infix (return TypeApp) P.AssocLeft ]
              , [ P.Infix (P.try rarrow >> return function) P.AssocRight ] ]
  postfixTable = [ \t -> KindedType t <$> (P.try doubleColon *> parseKind)
                 ]

-- |
-- Parse a monotype
--
parseType :: TokenParser u Type
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: TokenParser u Type
parsePolyType = parseAnyType

-- |
-- Parse an atomic type with no wildcards
--
noWildcards :: TokenParser u Type -> TokenParser u Type
noWildcards p = do
  ty <- p
  when (containsWildcards ty) $ P.unexpected "type wildcard"
  return ty

parseNameAndType :: TokenParser u t -> TokenParser u (String, t)
parseNameAndType p = (,) <$> (lname <|> stringLiteral) <* doubleColon <*> p

parseRowEnding :: TokenParser u Type
parseRowEnding = P.option REmpty (TypeVar <$> (pipe *> identifier))

parseRow :: TokenParser u Type
parseRow = (curry rowFromList <$> commaSep (parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
