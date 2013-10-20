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
import Control.Arrow (Arrow(..))

parseNumber :: P.Parsec String P.Column Type
parseNumber = const Number <$> P.string "Number"

parseString :: P.Parsec String P.Column Type
parseString = const String <$> P.string "String"

parseBoolean :: P.Parsec String P.Column Type
parseBoolean = const Boolean <$> P.string "Boolean"

parseArray :: P.Parsec String P.Column Type
parseArray = squares $ Array <$> parseType

parseObject :: P.Parsec String P.Column Type
parseObject = braces $ Object <$> parseRow

parseFunction :: P.Parsec String P.Column Type
parseFunction = do
  args <- lexeme $ parens $ commaSep parseType
  lexeme $ P.string "->"
  resultType <- parseType
  return $ Function args resultType

parseTypeVariable :: P.Parsec String P.Column Type
parseTypeVariable = TypeVar <$> identifier

parseTypeConstructor :: P.Parsec String P.Column Type
parseTypeConstructor = TypeConstructor <$> properName

parseTypeAtom :: P.Parsec String P.Column Type
parseTypeAtom = indented *> P.choice (map P.try
            [ parseNumber
            , parseString
            , parseBoolean
            , parseArray
            , parseObject
            , parseFunction
            , parseTypeVariable
            , parseTypeConstructor
            , parens parseType ])

parsePolyType :: P.Parsec String P.Column PolyType
parsePolyType = PolyType <$> (P.option [] (indented *> reserved "forall" *> many (indented *> identifier) <* indented <* dot))
                         <*> parseType

parseType :: P.Parsec String P.Column Type
parseType = fold (lexeme parseTypeAtom) (lexeme parseTypeAtom) TypeApp

parseNameAndType :: P.Parsec String P.Column (String, Type)
parseNameAndType = (,) <$> (indented *> identifier <* indented <* lexeme (P.string "::")) <*> parseType

parseRowEnding :: P.Parsec String P.Column Row
parseRowEnding = P.option REmpty (RowVar <$> (lexeme (indented *> P.char '|') *> indented *> identifier))

parseRow :: P.Parsec String P.Column Row
parseRow = fromList <$> (parseNameAndType `P.sepBy` (indented *> semi)) <*> parseRowEnding
  where
  fromList :: [(String, Type)] -> Row -> Row
  fromList [] r = r
  fromList ((name, t):ts) r = RCons name t (fromList ts r)
