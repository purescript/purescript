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
import Control.Monad
import Control.Arrow (Arrow(..))
import PureScript.Parser.Types
import PureScript.Types

booleanLiteral :: P.Parsec String P.Column Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String P.Column Value
parseNumericLiteral = NumericLiteral <$> C.integerOrFloat

parseStringLiteral :: P.Parsec String P.Column Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String P.Column Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: P.Parsec String P.Column Value
parseArrayLiteral = ArrayLiteral <$> (C.squares $ parseValue `P.sepBy` (C.indented *> C.comma))

parseObjectLiteral :: P.Parsec String P.Column Value
parseObjectLiteral = ObjectLiteral <$> (C.braces $ parseIdentifierAndValue `P.sepBy` (C.indented *> C.comma))

parseIdentifierAndValue :: P.Parsec String P.Column (String, Value)
parseIdentifierAndValue = (,) <$> (C.indented *> C.identifier <* C.indented <* C.colon)
                              <*> (C.indented *> parseValue)

parseAbs :: P.Parsec String P.Column Value
parseAbs = do
  C.lexeme $ P.char '\\'
  args <- (C.indented *> C.parseIdent) `P.sepBy` (C.indented *> C.comma)
  C.lexeme $ C.indented *> P.string "->"
  value <- parseValue
  return $ Abs args value

parseApp :: P.Parsec String P.Column Value
parseApp = App <$> parseValue
               <*> (C.indented *> C.parens (parseValue `P.sepBy` (C.indented *> C.comma)))

parseVar :: P.Parsec String P.Column Value
parseVar = Var <$> C.parseIdent

parseConstructor :: P.Parsec String P.Column Value
parseConstructor = Constructor <$> C.properName

parseCase :: P.Parsec String P.Column Value
parseCase = Case <$> (P.between (C.reserved "case") (C.indented *> C.reserved "of") parseValue)
                 <*> (C.indented *> C.mark (P.many (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: P.Parsec String P.Column (Binder, Value)
parseCaseAlternative = (,) <$> (parseBinder <* C.lexeme (P.string "->")) <*> parseValue

parseBlock :: P.Parsec String P.Column Value
parseBlock = Block <$> (C.reserved "do" *> parseManyStatements)

parseManyStatements :: P.Parsec String P.Column [Statement]
parseManyStatements = C.indented *> (C.mark $ P.many $ C.same *> C.mark parseStatement)

parseValueAtom :: P.Parsec String P.Column Value
parseValueAtom = C.indented *> P.choice (map P.try
            [ parseNumericLiteral
            , parseStringLiteral
            , parseBooleanLiteral
            , parseArrayLiteral
            , parseObjectLiteral
            , parseAbs
            , parseVar
            , parseConstructor
            , parseBlock
            , parseCase
            , C.parens parseValue ])

parseValue :: P.Parsec String P.Column Value
parseValue = buildExpressionParser operators . C.buildPostfixParser postfixTable2 $ indexersAndAccessors
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 $ parseValueAtom
  postfixTable1 = [ Accessor <$> (C.indented *> C.dot *> C.indented *> C.identifier)
                  , Indexer <$> (C.indented *> C.squares parseValue) ]
  postfixTable2 = [ C.indented *> indexersAndAccessors >>= \t2 -> return (\t1 -> App t1 [t2])
                  , flip App <$> (C.indented *> C.parens (parseValue `P.sepBy` (C.indented *> C.comma)))
                  , flip TypedValue <$> (P.try $ C.lexeme (C.indented *> P.string "::") *> parsePolyType) ]
  operators = [ [ Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "!") >> return (Unary Not)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "~") >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "-") >> return (Unary Negate)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "+") >> return id ]
              , [ Infix (C.lexeme (P.try (C.indented *> C.parseIdentInfix) >>= \ident -> return $ \t1 t2 -> App (App (Var ident) [t1]) [t2])) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "<=") >> return (Binary LessThanOrEqualTo)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp ">=") >> return (Binary GreaterThanOrEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "<") >> return (Binary LessThan)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp ">") >> return (Binary GreaterThan)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "*") >> return (Binary Multiply)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "/") >> return (Binary Divide)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "%") >> return (Binary Modulus)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "++") >> return (Binary Concat)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "+") >> return (Binary Add)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "-") >> return (Binary Subtract)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "<<") >> return (Binary ShiftLeft)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp ">>>") >> return (Binary ZeroFillShiftRight)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp ">>") >> return (Binary ShiftRight)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "==") >> return (Binary EqualTo)) AssocRight
                , Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "!=") >> return (Binary NotEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "&") >> return (Binary BitwiseAnd)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "^") >> return (Binary BitwiseXor)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "|") >> return (Binary BitwiseOr)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "&&") >> return (Binary And)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "||") >> return (Binary Or)) AssocRight ]
              ]

parseVariableIntroduction :: P.Parsec String P.Column Statement
parseVariableIntroduction = do
  C.reserved "var"
  name <- C.indented *> C.parseIdent
  C.lexeme $ C.indented *> P.char '='
  value <- parseValue
  return $ VariableIntroduction name value

parseAssignment :: P.Parsec String P.Column Statement
parseAssignment = do
  tgt <- parseAssignmentTarget
  C.lexeme $ C.indented *> P.char '='
  value <- parseValue
  return $ Assignment tgt value

parseWhile :: P.Parsec String P.Column Statement
parseWhile = While <$> (C.reserved "while" *> C.indented *> parseValue <* C.indented <* C.colon)
                   <*> parseManyStatements

parseFor :: P.Parsec String P.Column Statement
parseFor = For <$> (C.reserved "for" *> C.indented *> C.parseIdent)
               <*> (C.indented *> C.lexeme (P.string "<-") *> parseValue)
               <*> (C.indented *> C.reserved "until" *> parseValue <* C.colon)
               <*> parseManyStatements

parseForEach :: P.Parsec String P.Column Statement
parseForEach = ForEach <$> (C.reserved "foreach" *> C.indented *> C.parseIdent)
                       <*> (C.indented *> C.reserved "in" *> parseValue <* C.colon)
                       <*> parseManyStatements

parseIf :: P.Parsec String P.Column Statement
parseIf = If <$> parseIfStatement

parseIfStatement :: P.Parsec String P.Column IfStatement
parseIfStatement =
  IfStatement <$> (C.reserved "if" *> C.indented *> parseValue <* C.indented <* C.colon)
              <*> parseManyStatements
              <*> P.optionMaybe (C.same *> parseElseStatement)

parseElseStatement :: P.Parsec String P.Column ElseStatement
parseElseStatement = C.reserved "else" >> (ElseIf <$> (C.indented *> parseIfStatement)
                                           <|> Else <$> (C.indented *> C.colon *> parseManyStatements))

parseReturn :: P.Parsec String P.Column Statement
parseReturn = Return <$> (C.reserved "return" *> parseValue)

parseStatement :: P.Parsec String P.Column Statement
parseStatement = P.choice (map P.try
                 [ parseVariableIntroduction
                 , parseAssignment
                 , parseWhile
                 , parseFor
                 , parseForEach
                 , parseIf
                 , parseReturn ])

parseStringBinder :: P.Parsec String P.Column Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: P.Parsec String P.Column Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parsec String P.Column Binder
parseNumberBinder = NumberBinder <$> C.integerOrFloat

parseVarBinder :: P.Parsec String P.Column Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryBinder :: P.Parsec String P.Column Binder
parseNullaryBinder = NullaryBinder <$> C.lexeme C.properName

parseUnaryBinder :: P.Parsec String P.Column Binder
parseUnaryBinder = UnaryBinder <$> C.lexeme C.properName <*> (C.indented *> parseBinder)

parseObjectBinder :: P.Parsec String P.Column Binder
parseObjectBinder = ObjectBinder <$> C.braces ((C.indented *> parseIdentifierAndBinder) `P.sepBy` (C.indented *> C.comma))

parseArrayBinder :: P.Parsec String P.Column Binder
parseArrayBinder = C.squares $ ArrayBinder <$> ((C.indented *> parseBinder) `P.sepBy` (C.indented *> C.comma))
                                           <*> P.optionMaybe (C.indented *> C.colon *> C.indented *> parseBinder)

parseIdentifierAndBinder :: P.Parsec String P.Column (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  C.lexeme $ C.indented *> P.char '='
  binder <- C.indented *> parseBinder
  return (name, binder)

parseBinder :: P.Parsec String P.Column Binder
parseBinder = P.choice (map P.try
                  [ parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseVarBinder
                  , parseUnaryBinder
                  , parseNullaryBinder
                  , parseObjectBinder
                  , parseArrayBinder
                  , C.parens parseBinder ])

parseAssignmentTarget :: P.Parsec String P.Column AssignmentTarget
parseAssignmentTarget = buildExpressionParser operators (AssignVariable <$> C.parseIdent)
  where
  operators = [ [ Postfix $ AssignArrayIndex <$> (C.indented *> C.squares parseValue)
                , Postfix $ AssignObjectProperty <$> (C.indented *> C.dot *> C.indented *> C.identifier) ] ]
