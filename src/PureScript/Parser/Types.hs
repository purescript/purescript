-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Types
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript.Parser.Types (
    parseType,
    parseRow
) where

import PureScript.Types
import PureScript.Parser.Common
import Control.Applicative
import qualified Text.Parsec as P
import Control.Arrow (Arrow(..))

parseNumber :: P.Parsec String () Type
parseNumber = const Number <$> P.string "Number"

parseString :: P.Parsec String () Type
parseString = const String <$> P.string "String"

parseBoolean :: P.Parsec String () Type
parseBoolean = const Boolean <$> P.string "Boolean"

parseArray :: P.Parsec String () Type
parseArray = squares $ Array <$> parseType

parseObject :: P.Parsec String () Type
parseObject = braces $ Object <$> parseRow

parseFunction :: P.Parsec String () Type
parseFunction = do
  args <- lexeme $ parens $ commaSep parseType
  lexeme $ P.string "->"
  resultType <- parseType
  return $ Function args resultType

parseTypeVariable :: P.Parsec String () Type
parseTypeVariable = TypeVar <$> identifier

parseTypeConstructor :: P.Parsec String () Type
parseTypeConstructor = TypeConstructor <$> properName

parseTypeAtom :: P.Parsec String () Type
parseTypeAtom = P.choice $ map P.try
            [ parseNumber
            , parseString
            , parseBoolean
            , parseArray
            , parseObject
            , parseFunction
            , parseTypeVariable
            , parseTypeConstructor
            , parens parseType ]

parseType :: P.Parsec String () Type
parseType = fold (lexeme parseTypeAtom) (lexeme parseTypeAtom) TypeApp

parseREmpty :: P.Parsec String () Row
parseREmpty = braces whiteSpace >> return REmpty

parseRCons :: P.Parsec String () Row
parseRCons = RCons <$> (identifier <* lexeme (P.string "::"))
                   <*> (parseType <* semi)
                   <*> parseRow

parseRowVariable :: P.Parsec String () Row
parseRowVariable = RowVar <$> identifier

parseRow :: P.Parsec String () Row
parseRow = P.choice $ map P.try
       [ parseRCons
       , parseRowVariable
       , parseREmpty ]
