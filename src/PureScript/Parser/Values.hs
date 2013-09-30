-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Values
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

module PureScript.Parser.Values (
    parseValue,
    parseBinder
) where

import PureScript.Values
import qualified PureScript.Parser.Common as C
import Control.Applicative
import qualified Text.Parsec as P
import Text.Parsec.Expr
import Control.Arrow (Arrow(..))
import PureScript.Parser.Types
import PureScript.Types

parseNumericLiteral :: P.Parsec String () Value
parseNumericLiteral = NumericLiteral <$> either fromIntegral id <$> C.naturalOrFloat

parseStringLiteral :: P.Parsec String () Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String () Value
parseBooleanLiteral = BooleanLiteral <$> ((P.string "true" >> return True)
                                    P.<|> (P.string "false" >> return False))

parseArrayLiteral :: P.Parsec String () Value
parseArrayLiteral = ArrayLiteral <$> (C.squares $ C.commaSep parseValue)

parseObjectLiteral :: P.Parsec String () Value
parseObjectLiteral = ObjectLiteral <$> (C.braces $ C.commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: P.Parsec String () (String, Value)
parseIdentifierAndValue = do
  name <- C.lexeme C.identifier
  C.colon
  value <- parseValue
  return (name, value)

parseAbs :: P.Parsec String () Value
parseAbs = do
  P.char '\\'
  args <- C.commaSep1 C.identifier
  C.lexeme $ P.string "->"
  value <- parseValue
  return $ Abs args value

parseApp :: P.Parsec String () Value
parseApp = App <$> parseValue
               <*> (C.parens $ C.commaSep parseValue)

parseVar :: P.Parsec String () Value
parseVar = Var <$> C.identifier

parseCase :: P.Parsec String () Value
parseCase = Case <$> P.between (C.reserved "case") (C.reserved "of") parseValue
                      <*> C.braces (C.semiSep parseCaseAlternative)

parseCaseAlternative :: P.Parsec String () (Binder, Value)
parseCaseAlternative = (,) <$> (parseBinder <* C.lexeme (P.string "->")) <*> parseValue

parseBlock :: P.Parsec String () Value
parseBlock = Block <$> (C.braces $ P.many parseStatement)

parseValueAtom :: P.Parsec String () Value
parseValueAtom = P.choice $ map P.try
            [ parseNumericLiteral
            , parseStringLiteral
            , parseBooleanLiteral
            , parseArrayLiteral
            , parseObjectLiteral
            , parseAbs
            , parseVar
            , parseBlock
            , parseCase
            , C.parens parseValue ]

parseValue :: P.Parsec String () Value
parseValue = buildExpressionParser operators $ C.fold (C.lexeme typedValue') (C.lexeme funArgs) App
  where
  typedValue' = C.augment parseValueAtom parseTypeAnnotation TypedValue
  funArgs = C.parens $ C.commaSep parseValue
  parseTypeAnnotation = C.lexeme (P.string "::") *> parseType
  operators = [ [ Postfix $ (Accessor <$> (C.dot *> C.identifier)) ]
              , [ Infix (C.lexeme (P.try (P.string "||")) >> return (Binary Or)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.string "&&")) >> return (Binary And)) AssocRight ]
              , [ Infix (C.lexeme (P.char '|') >> return (Binary BitwiseOr)) AssocRight ]
              , [ Infix (C.lexeme (P.char '^') >> return (Binary BitwiseXor)) AssocRight ]
              , [ Infix (C.lexeme (P.char '&') >> return (Binary BitwiseAnd)) AssocRight ]
              , [ Infix (C.lexeme (P.string "==") >> return (Binary EqualTo)) AssocRight
                , Infix (C.lexeme (P.try (P.string "!=")) >> return (Binary NotEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.string "<<") >> return (Binary ShiftLeft)) AssocRight
                , Infix (C.lexeme (P.try (P.string ">>>")) >> return (Binary ZeroFillShiftRight)) AssocRight
                , Infix (C.lexeme (P.string ">>") >> return (Binary ShiftRight)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.string "++")) >> return (Binary Concat)) AssocRight
                , Infix (C.lexeme (P.char '+') >> return (Binary Add)) AssocRight
                , Infix (C.lexeme (P.char '-') >> return (Binary Subtract)) AssocRight ]
              , [ Infix (C.lexeme (P.char '*') >> return (Binary Multiply)) AssocRight
                , Infix (C.lexeme (P.char '/') >> return (Binary Divide)) AssocRight
                , Infix (C.lexeme (P.char '%') >> return (Binary Modulus)) AssocRight ]
              , [ Prefix $ C.lexeme (P.char '!') >> return (Unary Not)
                , Prefix $ C.lexeme (P.char '~') >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.char '-') >> return (Unary Negate) ]
              ]

parseVariableIntroduction :: P.Parsec String () Statement
parseVariableIntroduction = do
  C.reserved "var"
  name <- C.identifier
  C.lexeme $ P.char '='
  value <- parseValue
  C.semi
  return $ VariableIntroduction name value

parseAssignment :: P.Parsec String () (Statement)
parseAssignment = do
  tgt <- C.identifier
  C.lexeme $ P.char '='
  value <- parseValue
  C.semi
  return $ Assignment tgt value

parseManyStatements :: P.Parsec String () [Statement]
parseManyStatements = C.braces $ P.many parseStatement

parseWhile :: P.Parsec String () Statement
parseWhile = While <$> (C.reserved "while" *> C.parens parseValue)
                   <*> parseManyStatements

parseFor :: P.Parsec String () Statement
parseFor = For <$> (C.reserved "for" *> C.parens forIntro)
               <*> parseManyStatements
  where
  forIntro = (,,) <$> parseStatement
                  <*> (C.semi *> parseValue)
                  <*> (C.semi *> parseStatement)

parseIfThenElse :: P.Parsec String () Statement
parseIfThenElse = IfThenElse
                    <$> (C.reserved "if" *> C.parens parseValue)
                    <*> parseManyStatements
                    <*> P.optionMaybe (C.reserved "else" *> parseManyStatements)

parseReturn :: P.Parsec String () Statement
parseReturn = Return <$> (C.reserved "return" *> parseValue <* C.semi)

parseStatement :: P.Parsec String () Statement
parseStatement = P.choice $ map P.try
                 [ parseVariableIntroduction
                 , parseAssignment
                 , parseWhile
                 , parseFor
                 , parseIfThenElse
                 , parseReturn ]

parseVarBinder :: P.Parsec String () Binder
parseVarBinder = VarBinder <$> C.lexeme C.identifier

parseConstructorBinder :: P.Parsec String () Binder
parseConstructorBinder = ConstructorBinder <$> C.lexeme C.properName
                                           <*> parseBinder

parseObjectBinder :: P.Parsec String () Binder
parseObjectBinder = ObjectBinder <$> C.braces (C.commaSep parseIdentifierAndBinder)

parseIdentifierAndBinder :: P.Parsec String () (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  C.lexeme $ P.char '='
  binder <- parseBinder
  return (name, binder)

parseBinder :: P.Parsec String () Binder
parseBinder = P.choice $ map P.try
                  [ parseVarBinder
                  , parseConstructorBinder
                  , parseObjectBinder
                  , C.parens parseBinder ]
