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

booleanLiteral :: P.Parsec String () Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String () Value
parseNumericLiteral = NumericLiteral <$> C.naturalOrFloat

parseStringLiteral :: P.Parsec String () Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String () Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

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
  C.lexeme $ P.char '\\'
  args <- C.commaSep C.parseIdent
  C.lexeme $ P.string "->"
  value <- parseValue
  return $ Abs args value

parseApp :: P.Parsec String () Value
parseApp = App <$> parseValue
               <*> (C.parens $ C.commaSep parseValue)

parseVar :: P.Parsec String () Value
parseVar = Var <$> C.parseIdent

parseConstructor :: P.Parsec String () Value
parseConstructor = Constructor <$> C.properName

parseCase :: P.Parsec String () Value
parseCase = Case <$> P.between (C.reserved "case") (C.reserved "of") parseValue
                      <*> C.braces (C.semiSep parseCaseAlternative)

parseCaseAlternative :: P.Parsec String () (Binder, Value)
parseCaseAlternative = (,) <$> (parseBinder <* C.lexeme (P.string "->")) <*> parseValue

parseBlock :: P.Parsec String () Value
parseBlock = Block <$> (C.braces $ P.many (parseStatement True))

parseValueAtom :: P.Parsec String () Value
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

parseValue :: P.Parsec String () Value
parseValue = buildExpressionParser operators $ C.fold (C.lexeme typedValue) (C.lexeme funArgs) App
  where
  typedValue = C.augment parseValueAtom parseTypeAnnotation TypedValue
  funArgs = C.parens $ C.commaSep parseValue
  parseTypeAnnotation = C.lexeme (P.string "::") *> parsePolyType
  operators = [ [ Postfix $ Accessor <$> (C.dot *> C.identifier)
                , Postfix $ Indexer <$> C.squares parseValue ]
              , [ Prefix $ C.lexeme (P.char '!') >> return (Unary Not)
                , Prefix $ C.lexeme (P.char '~') >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.char '-') >> return (Unary Negate) ]
              , [ Infix (C.lexeme (C.parseIdentInfix >>= \ident -> return $ \t1 t2 -> App (App (Var ident) [t1]) [t2])) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.string "<=")) >> return (Binary LessThanOrEqualTo)) AssocRight
                , Infix (C.lexeme (P.try (P.string ">=")) >> return (Binary GreaterThanOrEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.char '<' <* P.notFollowedBy (P.char '<'))) >> return (Binary LessThan)) AssocRight
                , Infix (C.lexeme (P.try (P.char '>' <* P.notFollowedBy (P.char '>'))) >> return (Binary GreaterThan)) AssocRight ]
              , [ Infix (C.lexeme (P.char '*') >> return (Binary Multiply)) AssocRight
                , Infix (C.lexeme (P.char '/') >> return (Binary Divide)) AssocRight
                , Infix (C.lexeme (P.char '%') >> return (Binary Modulus)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.string "++")) >> return (Binary Concat)) AssocRight
                , Infix (C.lexeme (P.char '+') >> return (Binary Add)) AssocRight
                , Infix (C.lexeme (P.char '-') >> return (Binary Subtract)) AssocRight ]
              , [ Infix (C.lexeme (P.string "<<") >> return (Binary ShiftLeft)) AssocRight
                , Infix (C.lexeme (P.try (P.string ">>>")) >> return (Binary ZeroFillShiftRight)) AssocRight
                , Infix (C.lexeme (P.string ">>") >> return (Binary ShiftRight)) AssocRight ]
              , [ Infix (C.lexeme (P.string "==") >> return (Binary EqualTo)) AssocRight
                , Infix (C.lexeme (P.try (P.string "!=")) >> return (Binary NotEqualTo)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.char '&' <* P.notFollowedBy (P.char '&'))) >> return (Binary BitwiseAnd)) AssocRight ]
              , [ Infix (C.lexeme (P.char '^') >> return (Binary BitwiseXor)) AssocRight ]
              , [ Infix (C.lexeme (P.try (P.char '|' <* P.notFollowedBy (P.char '|'))) >> return (Binary BitwiseOr)) AssocRight ]
              , [ Infix (C.lexeme (P.string "&&") >> return (Binary And)) AssocRight ]
              , [ Infix (C.lexeme (P.string "||") >> return (Binary Or)) AssocRight ]
              ]

parseVariableIntroduction :: Bool -> P.Parsec String () Statement
parseVariableIntroduction requireSemi = do
  C.reserved "var"
  name <- C.parseIdent
  C.lexeme $ P.char '='
  value <- parseValue
  when requireSemi (void C.semi)
  return $ VariableIntroduction name value

parseAssignment :: Bool -> P.Parsec String () (Statement)
parseAssignment requireSemi = do
  tgt <- parseAssignmentTarget
  C.lexeme $ P.char '='
  value <- parseValue
  when requireSemi (void C.semi)
  return $ Assignment tgt value

parseManyStatements :: P.Parsec String () [Statement]
parseManyStatements = C.braces $ P.many (parseStatement True)

parseWhile :: P.Parsec String () Statement
parseWhile = While <$> (C.reserved "while" *> C.parens parseValue)
                   <*> parseManyStatements

parseFor :: P.Parsec String () Statement
parseFor = For <$> (C.reserved "for" *> C.parens forIntro)
               <*> parseManyStatements
  where
  forIntro = (,,) <$> parseStatement False
                  <*> (C.semi *> parseValue)
                  <*> (C.semi *> parseStatement False)

parseIfThenElse :: P.Parsec String () Statement
parseIfThenElse = IfThenElse
                    <$> (C.reserved "if" *> C.parens parseValue)
                    <*> parseManyStatements
                    <*> P.optionMaybe (C.reserved "else" *> parseManyStatements)

parseReturn :: Bool -> P.Parsec String () Statement
parseReturn requireSemi = Return <$> (C.reserved "return" *> parseValue <* when requireSemi (void C.semi))

parseStatement :: Bool -> P.Parsec String () Statement
parseStatement requireSemi = P.choice $ map P.try
                 [ parseVariableIntroduction requireSemi
                 , parseAssignment requireSemi
                 , parseWhile
                 , parseFor
                 , parseIfThenElse
                 , parseReturn requireSemi ]

parseStringBinder :: P.Parsec String () Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: P.Parsec String () Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parsec String () Binder
parseNumberBinder = NumberBinder <$> C.naturalOrFloat

parseVarBinder :: P.Parsec String () Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryBinder :: P.Parsec String () Binder
parseNullaryBinder = NullaryBinder <$> C.lexeme C.properName

parseUnaryBinder :: P.Parsec String () Binder
parseUnaryBinder = UnaryBinder <$> C.lexeme C.properName <*> parseBinder

parseObjectBinder :: P.Parsec String () Binder
parseObjectBinder = ObjectBinder <$> C.braces (C.commaSep parseIdentifierAndBinder)

parseArrayBinder :: P.Parsec String () Binder
parseArrayBinder = C.squares $ ArrayBinder <$> (C.commaSep parseBinder) <*> P.optionMaybe (C.colon *> parseBinder)

parseIdentifierAndBinder :: P.Parsec String () (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  C.lexeme $ P.char '='
  binder <- parseBinder
  return (name, binder)

parseBinder :: P.Parsec String () Binder
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

parseAssignmentTarget :: P.Parsec String () AssignmentTarget
parseAssignmentTarget = buildExpressionParser operators (AssignVariable <$> C.parseIdent)
  where
  operators = [ [ Postfix $ AssignArrayIndex <$> C.squares parseValue
                , Postfix $ AssignObjectProperty <$> (C.dot *> C.identifier) ] ]
