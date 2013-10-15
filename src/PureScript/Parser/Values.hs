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
import qualified Text.Parsec.Indent as I
import Text.Parsec.Expr
import Control.Monad
import Control.Arrow (Arrow(..))
import PureScript.Parser.Types
import PureScript.Types

booleanLiteral :: I.IndentParser String () Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: I.IndentParser String () Value
parseNumericLiteral = NumericLiteral <$> C.signedNumber

parseStringLiteral :: I.IndentParser String () Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: I.IndentParser String () Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: I.IndentParser String () Value
parseArrayLiteral = ArrayLiteral <$> (C.squares $ C.commaSep parseValue)

parseObjectLiteral :: I.IndentParser String () Value
parseObjectLiteral = ObjectLiteral <$> (C.braces $ C.commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: I.IndentParser String () (String, Value)
parseIdentifierAndValue = do
  name <- C.lexeme C.identifier
  C.colon
  value <- parseValue
  return (name, value)

parseAbs :: I.IndentParser String () Value
parseAbs = do
  C.lexeme $ P.char '\\'
  args <- C.commaSep C.parseIdent
  C.lexeme $ P.string "->"
  value <- parseValue
  return $ Abs args value

parseApp :: I.IndentParser String () Value
parseApp = App <$> parseValue
               <*> (C.parens $ C.commaSep parseValue)

parseVar :: I.IndentParser String () Value
parseVar = Var <$> C.parseIdent

parseConstructor :: I.IndentParser String () Value
parseConstructor = Constructor <$> C.properName

parseCase :: I.IndentParser String () Value
parseCase = Case <$> P.between (C.reserved "case") (C.reserved "of") parseValue
                 <*> I.withPos (C.indentedBlock parseCaseAlternative)

parseCaseAlternative :: I.IndentParser String () (Binder, Value)
parseCaseAlternative = (,) <$> (parseBinder <* C.lexeme (P.string "->")) <*> parseValue

parseBlock :: I.IndentParser String () Value
parseBlock = Block <$> (C.braces $ I.withPos $ C.indentedBlock $ parseStatement True)

parseValueAtom :: I.IndentParser String () Value
parseValueAtom = P.choice $ map P.try
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
            , C.parens parseValue ]

parseValue :: I.IndentParser String () Value
parseValue = buildExpressionParser operators $ C.fold (C.lexeme typedValue) (C.lexeme funArgs) App
  where
  typedValue = C.augment parseValueAtom parseTypeAnnotation TypedValue
  funArgs = C.parens $ C.commaSep parseValue
  parseTypeAnnotation = C.lexeme (P.string "::") *> parsePolyType
  operators = [ [ Postfix $ Accessor <$> (C.dot *> C.identifier)
                , Postfix $ Indexer <$> C.squares parseValue ]
              , [ Prefix $ C.lexeme (P.try $ C.reservedOp "!") >> return (Unary Not)
                , Prefix $ C.lexeme (P.try $ C.reservedOp "~") >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.try $ C.reservedOp "-") >> return (Unary Negate) ]
              , [ Infix (C.lexeme (P.try C.parseIdentInfix >>= \ident -> return $ \t1 t2 -> App (App (Var ident) [t1]) [t2])) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "<=") >> return (Binary LessThanOrEqualTo)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp ">=") >> return (Binary GreaterThanOrEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "<") >> return (Binary LessThan)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp ">") >> return (Binary GreaterThan)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "*") >> return (Binary Multiply)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp "/") >> return (Binary Divide)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp "%") >> return (Binary Modulus)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "++") >> return (Binary Concat)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp "+") >> return (Binary Add)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp "-") >> return (Binary Subtract)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "<<") >> return (Binary ShiftLeft)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp ">>>") >> return (Binary ZeroFillShiftRight)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp ">>") >> return (Binary ShiftRight)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "==") >> return (Binary EqualTo)) AssocRight
                , Infix (C.lexeme (P.try $ C.reservedOp "!=") >> return (Binary NotEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "&") >> return (Binary BitwiseAnd)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "^") >> return (Binary BitwiseXor)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "|") >> return (Binary BitwiseOr)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "&&") >> return (Binary And)) AssocRight ]
              , [ Infix (C.lexeme (P.try $ C.reservedOp "||") >> return (Binary Or)) AssocRight ]
              ]

parseVariableIntroduction :: Bool -> I.IndentParser String () Statement
parseVariableIntroduction requireSemi = do
  C.reserved "var"
  name <- C.parseIdent
  C.lexeme $ P.char '='
  value <- parseValue
  when requireSemi (void C.semi)
  return $ VariableIntroduction name value

parseAssignment :: Bool -> I.IndentParser String () (Statement)
parseAssignment requireSemi = do
  tgt <- parseAssignmentTarget
  C.lexeme $ P.char '='
  value <- parseValue
  when requireSemi (void C.semi)
  return $ Assignment tgt value

parseManyStatements :: I.IndentParser String () [Statement]
parseManyStatements = C.braces $ I.withPos $ C.indentedBlock $ parseStatement True

parseWhile :: I.IndentParser String () Statement
parseWhile = While <$> (C.reserved "while" *> C.parens parseValue)
                   <*> parseManyStatements

parseFor :: I.IndentParser String () Statement
parseFor = For <$> (C.reserved "for" *> C.parens forIntro)
               <*> parseManyStatements
  where
  forIntro = (,,) <$> parseStatement False
                  <*> (C.semi *> parseValue)
                  <*> (C.semi *> parseStatement False)

parseIfThenElse :: I.IndentParser String () Statement
parseIfThenElse = IfThenElse
                    <$> (C.reserved "if" *> C.parens parseValue)
                    <*> parseManyStatements
                    <*> P.optionMaybe (C.reserved "else" *> parseManyStatements)

parseReturn :: Bool -> I.IndentParser String () Statement
parseReturn requireSemi = Return <$> (C.reserved "return" *> parseValue <* when requireSemi (void C.semi))

parseStatement :: Bool -> I.IndentParser String () Statement
parseStatement requireSemi = P.choice $ map P.try
                 [ parseVariableIntroduction requireSemi
                 , parseAssignment requireSemi
                 , parseWhile
                 , parseFor
                 , parseIfThenElse
                 , parseReturn requireSemi ]

parseStringBinder :: I.IndentParser String () Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: I.IndentParser String () Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: I.IndentParser String () Binder
parseNumberBinder = NumberBinder <$> C.signedNumber

parseVarBinder :: I.IndentParser String () Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryBinder :: I.IndentParser String () Binder
parseNullaryBinder = NullaryBinder <$> C.lexeme C.properName

parseUnaryBinder :: I.IndentParser String () Binder
parseUnaryBinder = UnaryBinder <$> C.lexeme C.properName <*> parseBinder

parseObjectBinder :: I.IndentParser String () Binder
parseObjectBinder = ObjectBinder <$> C.braces (C.commaSep parseIdentifierAndBinder)

parseArrayBinder :: I.IndentParser String () Binder
parseArrayBinder = C.squares $ ArrayBinder <$> (C.commaSep parseBinder) <*> P.optionMaybe (C.colon *> parseBinder)

parseIdentifierAndBinder :: I.IndentParser String () (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  C.lexeme $ P.char '='
  binder <- parseBinder
  return (name, binder)

parseBinder :: I.IndentParser String () Binder
parseBinder = P.choice $ map P.try
                  [ parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseVarBinder
                  , parseUnaryBinder
                  , parseNullaryBinder
                  , parseObjectBinder
                  , parseArrayBinder
                  , C.parens parseBinder ]

parseAssignmentTarget :: I.IndentParser String () AssignmentTarget
parseAssignmentTarget = buildExpressionParser operators (AssignVariable <$> C.parseIdent)
  where
  operators = [ [ Postfix $ AssignArrayIndex <$> C.squares parseValue
                , Postfix $ AssignObjectProperty <$> (C.dot *> C.identifier) ] ]
