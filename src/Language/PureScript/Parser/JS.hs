-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.JS
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

module Language.PureScript.Parser.JS (
    parseJS
) where

import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.AST
import qualified Language.PureScript.Parser.Common as C
import Control.Applicative
import Data.Functor.Identity
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as P

booleanLiteral :: P.Parsec String u Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String u JS
parseNumericLiteral = JSNumericLiteral <$> C.integerOrFloat

parseStringLiteral :: P.Parsec String u JS
parseStringLiteral = JSStringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String u JS
parseBooleanLiteral = JSBooleanLiteral <$> booleanLiteral

parseArrayLiteral :: P.Parsec String u JS
parseArrayLiteral = JSArrayLiteral <$> P.squares C.tokenParser (P.commaSep C.tokenParser parseJS)

parseObjectLiteral :: P.Parsec String u JS
parseObjectLiteral = JSObjectLiteral <$> P.braces C.tokenParser (P.commaSep C.tokenParser parseIdentifierAndValue)

parseIdentifierAndValue :: P.Parsec String u (String, JS)
parseIdentifierAndValue = (,) <$> (C.identifier <* C.colon)
                              <*> parseJS

parseFunction :: P.Parsec String u JS
parseFunction = do
  C.reserved "function"
  name <- P.optionMaybe (Ident <$> C.identifier)
  args <- P.parens C.tokenParser $ P.commaSep C.tokenParser (Ident <$> C.identifier)
  body <- parseJS
  return $ JSFunction name args body

parseBlock :: P.Parsec String u JS
parseBlock = JSBlock <$> P.braces C.tokenParser (P.many parseJS)

parseVar :: P.Parsec String u JS
parseVar = JSVar <$> Ident <$> C.identifier

parseJSAtom :: P.Parsec String u JS
parseJSAtom = P.choice
            [ P.try parseNumericLiteral
            , P.try parseStringLiteral
            , P.try parseBooleanLiteral
            , parseArrayLiteral
            , P.try parseObjectLiteral
            , parseFunction
            , parseBlock
            , P.try parseVar
            , parseVariableIntroduction
            , P.try parseAssignment
            , parseWhile
            , parseIf
            , parseReturn
            , P.parens C.tokenParser parseJS ]

parseAccessor :: JS -> P.Parsec String u JS
parseAccessor js = P.try $ flip JSAccessor js <$> (C.dot *> P.notFollowedBy C.opLetter *> C.identifier)

parseIndexer :: JS -> P.Parsec String u JS
parseIndexer js = P.try $ flip JSIndexer js <$> (P.squares C.tokenParser parseJS)

parseConditional :: JS -> P.Parsec String u JS
parseConditional js = P.try $ do
  _ <- C.lexeme $ P.char '?'
  tr <- parseJS
  _ <- C.lexeme $ P.char ':'
  fa <- parseJS
  return $ JSConditional js tr fa

binary :: BinaryOperator -> String -> P.Assoc -> P.Operator String u Identity JS
binary op s f = P.Infix (P.try $ C.reservedOp s >> return (JSBinary op)) f

unary :: UnaryOperator -> String -> P.Operator String u Identity JS
unary op s = P.Prefix (P.try $ C.reservedOp s >> return (JSUnary op))

parseJS :: P.Parsec String u JS
parseJS =
  (P.buildExpressionParser operators
   . C.buildPostfixParser postfixTable2
   $ indexersAndAccessors) P.<?> "javascript"
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 parseJSAtom
  postfixTable1 = [ parseAccessor, parseIndexer, parseConditional ]
  postfixTable2 = [ \v -> P.try $ JSApp v <$> (P.parens C.tokenParser (P.commaSep C.tokenParser parseJS)) ]
  operators = [ [ binary    LessThan             "<"     P.AssocLeft]
              , [ binary    LessThanOrEqualTo    "<="    P.AssocLeft]
              , [ binary    GreaterThan          ">"     P.AssocLeft]
              , [ binary    GreaterThanOrEqualTo ">="    P.AssocLeft]
              , [ unary     Not                  "!" ]
              , [ unary     BitwiseNot           "~" ]
              , [ unary     Negate               "-" ]
              , [ unary     Positive             "+" ]
              , [ binary    Multiply             "*"     P.AssocLeft]
              , [ binary    Divide               "/"     P.AssocLeft]
              , [ binary    Modulus              "%"     P.AssocLeft]
              , [ binary    Concat               "+"     P.AssocLeft]
              , [ binary    Add                  "+"     P.AssocLeft]
              , [ binary    Subtract             "-"     P.AssocLeft]
              , [ binary    ShiftLeft            "<<"    P.AssocLeft]
              , [ binary    ShiftRight           ">>"    P.AssocLeft]
              , [ binary    ZeroFillShiftRight   ">>>"   P.AssocLeft]
              , [ binary    EqualTo              "==="   P.AssocLeft]
              , [ binary    NotEqualTo           "!=="   P.AssocLeft]
              , [ binary    BitwiseAnd           "&"     P.AssocLeft]
              , [ binary    BitwiseXor           "^"     P.AssocLeft]
              , [ binary    BitwiseOr            "|"     P.AssocLeft]
              , [ binary    And                  "&&"    P.AssocRight]
              , [ binary    Or                   "||"    P.AssocRight]
              ]

parseVariableIntroduction :: P.Parsec String u JS
parseVariableIntroduction = do
  C.reserved "var"
  name <- Ident <$> P.identifier C.tokenParser
  value <- P.optionMaybe $ do
    _ <- C.lexeme $ P.char '='
    value <- parseJS
    _ <- C.semi
    return value
  return $ JSVariableIntroduction name value

parseAssignment :: P.Parsec String u JS
parseAssignment = do
  tgt <- parseAssignmentTarget
  _ <- C.lexeme $ P.char '='
  value <- parseJS
  _ <- C.semi
  return $ JSAssignment tgt value

parseAssignmentTarget :: P.Parsec String u JSAssignment
parseAssignmentTarget = C.buildPostfixParser [] (JSAssignVariable <$> Ident <$> P.identifier C.tokenParser)

parseWhile :: P.Parsec String u JS
parseWhile = JSWhile <$> (C.reserved "while" *> P.parens C.tokenParser parseJS)
                     <*> parseJS

parseIf :: P.Parsec String u JS
parseIf = JSIfElse <$> (C.reserved "if" *> P.parens C.tokenParser parseJS)
                   <*> parseJS
                   <*> P.optionMaybe (C.reserved "else" >> parseJS)

parseReturn :: P.Parsec String u JS
parseReturn = JSReturn <$> (C.reserved "return" *> parseJS <* C.semi)
