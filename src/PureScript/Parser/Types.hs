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

parseForAllType :: P.Parsec String () Type
parseForAllType = ForAll <$> (reserved "forall" *> many identifier)
                         <*> (dot *> parseType)

parseTypeAtom :: P.Parsec String () Type
parseTypeAtom = P.choice $ map P.try
            [ parseNumber
            , parseString
            , parseBoolean
            , parseArray
            , parseObject
            , parseFunction
            , parseForAllType
            , parseTypeVariable
            , parseTypeConstructor
            , parens parseType ]

parseType :: P.Parsec String () Type
parseType = fold (lexeme parseTypeAtom) (lexeme parseTypeAtom) TypeApp

parseNameAndType :: P.Parsec String () (String, Type)
parseNameAndType = (,) <$> (identifier <* lexeme (P.string "::")) <*> parseType

parseRowEnding :: P.Parsec String () Row
parseRowEnding = P.option REmpty (RowVar <$> (lexeme (P.char '|') *> identifier))

parseRow :: P.Parsec String () Row
parseRow = fromList <$> semiSep parseNameAndType <*> parseRowEnding
  where
  fromList :: [(String, Type)] -> Row -> Row
  fromList [] r = r
  fromList ((name, t):ts) r = RCons name t (fromList ts r)
