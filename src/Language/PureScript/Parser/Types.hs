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
    parseTypeAtom
) where

import Control.Applicative
import Control.Monad (when, unless)

import Language.PureScript.Types
import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Language.PureScript.Prim

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseNumber :: P.Parsec String ParseState Type
parseNumber = const tyNumber <$> reserved "Number"

parseString :: P.Parsec String ParseState Type
parseString = const tyString <$> reserved "String"

parseBoolean :: P.Parsec String ParseState Type
parseBoolean = const tyBoolean <$> reserved "Boolean"

parseArray :: P.Parsec String ParseState Type
parseArray = squares $ return tyArray

parseArrayOf :: P.Parsec String ParseState Type
parseArrayOf = squares $ TypeApp tyArray <$> parseType

parseFunction :: P.Parsec String ParseState Type
parseFunction = parens $ P.try (lexeme (P.string "->")) >> return tyFunction

parseObject :: P.Parsec String ParseState Type
parseObject = braces $ Object <$> parseRow False

parseTypeVariable :: P.Parsec String ParseState Type
parseTypeVariable = do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected ident
  return $ TypeVar ident

parseTypeConstructor :: P.Parsec String ParseState Type
parseTypeConstructor = TypeConstructor <$> parseQualified properName

parseForAll :: P.Parsec String ParseState Type
parseForAll = mkForAll <$> (P.try (reserved "forall") *> P.many1 (indented *> identifier) <* indented <* dot)
                       <*> parseConstrainedType

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: P.Parsec String ParseState Type
parseTypeAtom = indented *> P.choice (map P.try
            [ parseNumber
            , parseString
            , parseBoolean
            , parseArray
            , parseArrayOf
            , parseFunction
            , parseObject
            , parseTypeVariable
            , parseTypeConstructor
            , parseForAll
            , parens (parseRow True)
            , parens parsePolyType ])

parseConstrainedType :: P.Parsec String ParseState Type
parseConstrainedType = do
  constraints <- P.optionMaybe . P.try $ do
    constraints <- parens . commaSep1 $ do
      className <- parseQualified properName
      indented
      ty <- P.many parseTypeAtom
      return (className, ty)
    _ <- lexeme $ P.string "=>"
    return constraints
  indented
  ty <- parseType
  return $ maybe ty (flip ConstrainedType ty) constraints

parseAnyType :: P.Parsec String ParseState Type
parseAnyType = P.buildExpressionParser operators parseTypeAtom P.<?> "type"
  where
  operators = [ [ P.Infix (return TypeApp) P.AssocLeft ]
              , [ P.Infix (P.try (lexeme (P.string "->")) >> return function) P.AssocRight ] ]

-- |
-- Parse a monotype
--
parseType :: P.Parsec String ParseState Type
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: P.Parsec String ParseState Type
parsePolyType = parseAnyType

parseNameAndType :: P.Parsec String ParseState t -> P.Parsec String ParseState (String, t)
parseNameAndType p = (,) <$> (indented *> identifier <* indented <* lexeme (P.string "::")) <*> p

parseRowEnding :: P.Parsec String ParseState Type
parseRowEnding = P.option REmpty (TypeVar <$> (lexeme (indented *> P.char '|') *> indented *> identifier))

parseRow :: Bool -> P.Parsec String ParseState Type
parseRow nonEmpty = (curry rowFromList <$> many' (parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
  where many' = if nonEmpty then commaSep1 else commaSep
