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
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Types (
    parseType,
    parsePolyType,
    parseRow
) where

import Language.PureScript.Types
import Language.PureScript.Parser.State
import Language.PureScript.Parser.Common
import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P
import Control.Monad (unless)

parseNumber :: P.Parsec String ParseState Type
parseNumber = const Number <$> reserved "Number"

parseString :: P.Parsec String ParseState Type
parseString = const String <$> reserved "String"

parseBoolean :: P.Parsec String ParseState Type
parseBoolean = const Boolean <$> reserved "Boolean"

parseArray :: P.Parsec String ParseState Type
parseArray = squares $ Array <$> parseType

parseObject :: P.Parsec String ParseState Type
parseObject = braces $ Object <$> parseRow

parseFunction :: P.Parsec String ParseState Type
parseFunction = do
  args <- lexeme $ parens $ commaSep parsePolyType
  _ <- lexeme $ P.string "->"
  resultType <- parseType
  return $ Function args resultType

parseTypeVariable :: P.Parsec String ParseState Type
parseTypeVariable = TypeVar <$> identifier

parseTypeConstructor :: P.Parsec String ParseState Type
parseTypeConstructor = TypeConstructor <$> parseQualified properName

parseForAll :: P.Parsec String ParseState Type
parseForAll = (mkForAll <$> (P.try (reserved "forall") *> P.many1 (indented *> identifier) <* indented <* dot)
                        <*> parseConstrainedType)

parseTypeAtom :: P.Parsec String ParseState Type
parseTypeAtom = indented *> P.choice (map P.try
            [ parseNumber
            , parseString
            , parseBoolean
            , parseArray
            , parseObject
            , parseFunction
            , parseTypeVariable
            , parseTypeConstructor
            , parseForAll
            , parens parseRow
            , parens parseType ])

parseConstrainedType :: P.Parsec String ParseState Type
parseConstrainedType = do
  constraints <- P.optionMaybe . P.try $ do
    constraints <- parens . commaSep1 $ do
      className <- parseQualified properName
      indented
      ty <- parseType
      return (className, ty)
    _ <- lexeme $ P.string "=>"
    return constraints
  indented
  ty <- parseType
  return $ maybe ty (flip ConstrainedType ty) constraints

parseAnyType :: P.Parsec String ParseState Type
parseAnyType = (P.buildExpressionParser operators . buildPostfixParser postfixTable $ parseTypeAtom) P.<?> "type"
  where
  postfixTable :: [Type -> P.Parsec String ParseState Type]
  postfixTable = [ \x -> TypeApp x <$> P.try (indented *> parseTypeAtom) ]
  operators = [ [ P.Infix (lexeme (P.try (P.string "->")) >> return (\t1 t2 -> Function [t1] t2)) P.AssocRight ] ]

parseType :: P.Parsec String ParseState Type
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

parsePolyType :: P.Parsec String ParseState Type
parsePolyType = do
  ty <- parseAnyType
  unless (isPolyType ty) $ P.unexpected "polymorphic type"
  return ty

parseNameAndType :: P.Parsec String ParseState t -> P.Parsec String ParseState (String, t)
parseNameAndType p = (,) <$> (indented *> identifier <* indented <* lexeme (P.string "::")) <*> p

parseRowEnding :: P.Parsec String ParseState Type
parseRowEnding = P.option REmpty (TypeVar <$> (lexeme (indented *> P.char '|') *> indented *> identifier))

parseRow :: P.Parsec String ParseState Type
parseRow = (fromList <$> (commaSep $ parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
  where
  fromList :: [(String, Type)] -> Type -> Type
  fromList [] r = r
  fromList ((name, t):ts) r = RCons name t (fromList ts r)
