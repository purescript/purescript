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
    parsePolyType,
    parseRow
) where

import PureScript.Types
import PureScript.Parser.Common
import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.Indent as I
import Control.Arrow (Arrow(..))

parseNumber :: I.IndentParser String () Type
parseNumber = const Number <$> P.string "Number"

parseString :: I.IndentParser String () Type
parseString = const String <$> P.string "String"

parseBoolean :: I.IndentParser String () Type
parseBoolean = const Boolean <$> P.string "Boolean"

parseArray :: I.IndentParser String () Type
parseArray = squares $ Array <$> parseType

parseObject :: I.IndentParser String () Type
parseObject = braces $ Object <$> parseRow

parseFunction :: I.IndentParser String () Type
parseFunction = do
  args <- lexeme $ parens $ commaSep parseType
  lexeme $ P.string "->"
  resultType <- parseType
  return $ Function args resultType

parseTypeVariable :: I.IndentParser String () Type
parseTypeVariable = TypeVar <$> identifier

parseTypeConstructor :: I.IndentParser String () Type
parseTypeConstructor = TypeConstructor <$> properName

parseTypeAtom :: I.IndentParser String () Type
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

parsePolyType :: I.IndentParser String () PolyType
parsePolyType = PolyType <$> (P.option [] (reserved "forall" *> many identifier <* dot))
                         <*> parseType

parseType :: I.IndentParser String () Type
parseType = fold (lexeme parseTypeAtom) (lexeme parseTypeAtom) TypeApp

parseNameAndType :: I.IndentParser String () (String, Type)
parseNameAndType = (,) <$> (identifier <* lexeme (P.string "::")) <*> parseType

parseRowEnding :: I.IndentParser String () Row
parseRowEnding = P.option REmpty (RowVar <$> (lexeme (P.char '|') *> identifier))

parseRow :: I.IndentParser String () Row
parseRow = fromList <$> semiSep parseNameAndType <*> parseRowEnding
  where
  fromList :: [(String, Type)] -> Row -> Row
  fromList [] r = r
  fromList ((name, t):ts) r = RCons name t (fromList ts r)
