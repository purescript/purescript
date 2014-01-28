-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Values
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parsers for values, statements, binders and guards
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Values (
    parseValue,
    parseStatement,
    parseGuard,
    parseBinder,
    parseBinderNoParens,
) where

import Language.PureScript.Values
import Language.PureScript.Parser.State
import qualified Language.PureScript.Parser.Common as C
import Control.Applicative
import qualified Text.Parsec as P
import Text.Parsec.Expr
import Language.PureScript.Parser.Types

booleanLiteral :: P.Parsec String ParseState Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String ParseState Value
parseNumericLiteral = NumericLiteral <$> C.integerOrFloat

parseStringLiteral :: P.Parsec String ParseState Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String ParseState Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: P.Parsec String ParseState Value
parseArrayLiteral = ArrayLiteral <$> C.squares (C.commaSep parseValue)

parseObjectLiteral :: P.Parsec String ParseState Value
parseObjectLiteral = ObjectLiteral <$> C.braces (C.commaSep parseIdentifierAndValue)

parseIdentifierAndValue :: P.Parsec String ParseState (String, Value)
parseIdentifierAndValue = (,) <$> (C.indented *> C.identifier <* C.indented <* C.colon)
                              <*> (C.indented *> parseValue)

parseAbs :: P.Parsec String ParseState Value
parseAbs = do
  C.reservedOp "\\"
  args <- P.many (C.indented *> (P.try singleArg <|> manyArgs))
  C.indented *> C.reservedOp "->"
  value <- parseValue
  return $ toFunction args value
  where
  manyArgs :: P.Parsec String ParseState (Value -> Value)
  manyArgs = do
    args <- C.parens (C.commaSep C.parseIdent)
    return $ Abs args
  singleArg :: P.Parsec String ParseState (Value -> Value)
  singleArg = Abs . return <$> C.parseIdent
  toFunction :: [Value -> Value] -> Value -> Value
  toFunction [] value = Abs [] value
  toFunction args value = foldr (($)) value args

parseVar :: P.Parsec String ParseState Value
parseVar = Var <$> C.parseQualified C.parseIdent

parseConstructor :: P.Parsec String ParseState Value
parseConstructor = Constructor <$> C.parseQualified C.properName

parseCase :: P.Parsec String ParseState Value
parseCase = Case <$> P.between (P.try (C.reserved "case")) (C.indented *> C.reserved "of") (return <$> parseValue)
                 <*> (C.indented *> C.mark (P.many (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: P.Parsec String ParseState ([Binder], Maybe Guard, Value)
parseCaseAlternative = (,,) <$> (return <$> parseBinder)
                            <*> P.optionMaybe parseGuard
                            <*> (C.indented *> C.reservedOp "->" *> parseValue)
                            P.<?> "case alternative"

parseIfThenElse :: P.Parsec String ParseState Value
parseIfThenElse = IfThenElse <$> (P.try (C.reserved "if") *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "then" *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "else" *> C.indented *> parseValue)

parseBlock :: P.Parsec String ParseState Value
parseBlock = Block <$> parseManyStatements

parseManyStatements :: P.Parsec String ParseState [Statement]
parseManyStatements = (do
  _ <- C.lexeme $ P.char '{'
  C.indented
  sts <- C.mark (P.many (C.same *> C.mark parseStatement))
  _ <- C.lexeme (P.char '}')
  return sts) P.<?> "block"

parseValueAtom :: P.Parsec String ParseState Value
parseValueAtom = P.choice
            [ P.try parseNumericLiteral
            , P.try parseStringLiteral
            , P.try parseBooleanLiteral
            , parseArrayLiteral
            , P.try parseObjectLiteral
            , parseAbs
            , P.try parseConstructor
            , P.try parseVar
            , parseBlock
            , parseCase
            , parseIfThenElse
            , parseDo
            , Parens <$> C.parens parseValue ]

parsePropertyUpdate :: P.Parsec String ParseState (String, Value)
parsePropertyUpdate = do
  name <- C.lexeme C.identifier
  _ <- C.lexeme $ C.indented *> P.char '='
  value <- C.indented *> parseValue
  return (name, value)

parseAccessor :: Value -> P.Parsec String ParseState Value
parseAccessor (Constructor _) = P.unexpected "constructor"
parseAccessor obj = P.try $ Accessor <$> (C.indented *> C.dot *> P.notFollowedBy C.opLetter *> C.indented *> C.identifier) <*> pure obj

parseDo :: P.Parsec String ParseState Value
parseDo = do
  C.reserved "do"
  C.indented
  Do <$> C.mark (P.many (C.same *> C.mark parseDoNotationElement))

parseDoNotationLet :: P.Parsec String ParseState DoNotationElement
parseDoNotationLet = DoNotationLet <$> (C.reserved "let" *> C.indented *> parseBinder)
                                   <*> (C.indented *> C.reservedOp "=" *> parseValue)

parseDoNotationBind :: P.Parsec String ParseState DoNotationElement
parseDoNotationBind = DoNotationBind <$> parseBinder <*> (C.indented *> C.reservedOp "<-" *> parseValue)

parseDoNotationElement :: P.Parsec String ParseState DoNotationElement
parseDoNotationElement = P.choice
            [ P.try parseDoNotationBind
            , parseDoNotationLet
            , P.try (DoNotationValue <$> parseValue) ]

-- |
-- Parse a value
--
parseValue :: P.Parsec String ParseState Value
parseValue =
  (buildExpressionParser operators
   . C.buildPostfixParser postfixTable2
   $ indexersAndAccessors) P.<?> "expression"
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 parseValueAtom
  postfixTable1 = [ parseAccessor
                  , \v -> P.try $ flip ObjectUpdate <$> (C.indented *> C.braces (C.commaSep1 (C.indented *> parsePropertyUpdate))) <*> pure v ]
  postfixTable2 = [ \v -> P.try (C.indented *> indexersAndAccessors >>= \t2 -> return (\t1 -> App t1 [t2])) <*> pure v
                  , \v -> P.try $ flip App <$> (C.indented *> C.parens (C.commaSep parseValue)) <*> pure v
                  , \v -> flip (TypedValue True) <$> (P.try (C.lexeme (C.indented *> P.string "::")) *> parsePolyType) <*> pure v
                  ]
  operators = [ [ Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "!") >> return (Unary Not)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "~") >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "-") >> return (Unary Negate)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "+") >> return id ]
              , [ Infix (C.lexeme (P.try (C.indented *> C.parseIdentInfix P.<?> "operator") >>= \ident ->
                    return (BinaryNoParens ident))) AssocRight ]
              ]

parseVariableIntroduction :: P.Parsec String ParseState Statement
parseVariableIntroduction = do
  C.reserved "var"
  name <- C.indented *> C.parseIdent
  _ <- C.lexeme $ C.indented *> P.char '='
  value <- parseValue
  _ <- C.indented *> C.semi
  return $ VariableIntroduction name value

parseAssignment :: P.Parsec String ParseState Statement
parseAssignment = do
  tgt <- P.try $ do
    tgt <- C.parseIdent
    _ <- C.lexeme $ C.indented *> P.char '='
    return tgt
  value <- parseValue
  _ <- C.indented *> C.semi
  return $ Assignment tgt value

parseWhile :: P.Parsec String ParseState Statement
parseWhile = While <$> (C.reserved "while" *> C.indented *> C.parens parseValue)
                   <*> (C.indented *> parseManyStatements)

parseFor :: P.Parsec String ParseState Statement
parseFor = For <$> (C.reserved "for" *> C.indented *> C.lexeme (P.char '(') *> C.indented *> C.parseIdent)
               <*> (C.indented *> C.lexeme (P.string "<-") *> parseValue)
               <*> (C.indented *> C.reserved "until" *> parseValue <* C.indented <* C.lexeme (P.char ')'))
               <*> parseManyStatements

parseIf :: P.Parsec String ParseState Statement
parseIf = If <$> parseIfStatement

parseIfStatement :: P.Parsec String ParseState IfStatement
parseIfStatement =
  IfStatement <$> (C.reserved "if" *> C.indented *> C.parens parseValue)
              <*> parseManyStatements
              <*> P.optionMaybe parseElseStatement

parseElseStatement :: P.Parsec String ParseState ElseStatement
parseElseStatement = C.reserved "else" >> (ElseIf <$> parseIfStatement
                                           <|> Else <$> parseManyStatements)

parseReturn :: P.Parsec String ParseState Statement
parseReturn = Return <$> (C.reserved "return" *> parseValue <* C.indented <* C.semi)

-- |
-- Parse a statement
--
parseStatement :: P.Parsec String ParseState Statement
parseStatement = P.choice
                 [ parseVariableIntroduction
                 , parseAssignment
                 , parseWhile
                 , parseFor
                 , parseIf
                 , parseReturn ] P.<?> "statement"

parseStringBinder :: P.Parsec String ParseState Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: P.Parsec String ParseState Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parsec String ParseState Binder
parseNumberBinder = NumberBinder <$> C.integerOrFloat

parseVarBinder :: P.Parsec String ParseState Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryBinder :: P.Parsec String ParseState Binder
parseNullaryBinder = NullaryBinder <$> C.lexeme (C.parseQualified C.properName)

parseUnaryBinder :: P.Parsec String ParseState Binder
parseUnaryBinder = UnaryBinder <$> C.lexeme (C.parseQualified C.properName) <*> (C.indented *> parseBinder)

parseObjectBinder :: P.Parsec String ParseState Binder
parseObjectBinder = ObjectBinder <$> C.braces (C.commaSep (C.indented *> parseIdentifierAndBinder))

parseArrayBinder :: P.Parsec String ParseState Binder
parseArrayBinder = C.squares $ ArrayBinder <$> (C.commaSep (C.indented *> parseBinder))

parseNamedBinder :: P.Parsec String ParseState Binder
parseNamedBinder = NamedBinder <$> (C.parseIdent <* C.indented <* C.lexeme (P.char '@'))
                               <*> (C.indented *> parseBinder)

parseNullBinder :: P.Parsec String ParseState Binder
parseNullBinder = C.lexeme (P.char '_') *> P.notFollowedBy C.identLetter *> return NullBinder

parseIdentifierAndBinder :: P.Parsec String ParseState (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  _ <- C.lexeme $ C.indented *> P.char '='
  binder <- C.indented *> parseBinder
  return (name, binder)

parseBinderAtom :: P.Parsec String ParseState Binder
parseBinderAtom = P.choice (map P.try
                  [ parseNullBinder
                  , parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseNamedBinder
                  , parseVarBinder
                  , parseUnaryBinder
                  , parseNullaryBinder
                  , parseObjectBinder
                  , parseArrayBinder
                  , C.parens parseBinder ]) P.<?> "binder"

-- |
-- Parse a binder
--
parseBinder :: P.Parsec String ParseState Binder
parseBinder = (buildExpressionParser operators parseBinderAtom) P.<?> "expression"
  where
  operators = [ [ Infix ( C.lexeme (P.try $ C.indented *> C.reservedOp ":") >> return ConsBinder) AssocRight ] ]

-- |
-- Parse a binder as it would appear in a top level declaration
--
parseBinderNoParens :: P.Parsec String ParseState Binder
parseBinderNoParens = P.choice (map P.try
                  [ parseNullBinder
                  , parseStringBinder
                  , parseBooleanBinder
                  , parseNumberBinder
                  , parseNamedBinder
                  , parseVarBinder
                  , parseNullaryBinder
                  , parseObjectBinder
                  , parseArrayBinder
                  , C.parens parseBinder ]) P.<?> "binder"
-- |
-- Parse a guard
--
parseGuard :: P.Parsec String ParseState Guard
parseGuard = C.indented *> C.pipe *> C.indented *> parseValue

