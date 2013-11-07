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
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Values (
    parseValue,
    parseBinder
) where

import Language.PureScript.Values
import Language.PureScript.Names
import Language.PureScript.Declarations
import Language.PureScript.Parser.State
import Data.Function (on)
import Data.List
import Data.Functor.Identity
import qualified Data.Map as M
import qualified Language.PureScript.Parser.Common as C
import Control.Applicative
import qualified Text.Parsec as P
import Text.Parsec.Expr
import Control.Monad
import Control.Arrow (Arrow(..))
import Language.PureScript.Parser.Types
import Language.PureScript.Types

booleanLiteral :: P.Parsec String ParseState Bool
booleanLiteral = (C.reserved "true" >> return True) P.<|> (C.reserved "false" >> return False)

parseNumericLiteral :: P.Parsec String ParseState Value
parseNumericLiteral = NumericLiteral <$> C.integerOrFloat

parseStringLiteral :: P.Parsec String ParseState Value
parseStringLiteral = StringLiteral <$> C.stringLiteral

parseBooleanLiteral :: P.Parsec String ParseState Value
parseBooleanLiteral = BooleanLiteral <$> booleanLiteral

parseArrayLiteral :: P.Parsec String ParseState Value
parseArrayLiteral = ArrayLiteral <$> C.squares (parseValue `P.sepBy` (C.indented *> C.comma))

parseObjectLiteral :: P.Parsec String ParseState Value
parseObjectLiteral = ObjectLiteral <$> C.braces (parseIdentifierAndValue `P.sepBy` (C.indented *> C.comma))

parseIdentifierAndValue :: P.Parsec String ParseState (String, Value)
parseIdentifierAndValue = (,) <$> (C.indented *> C.identifier <* C.indented <* C.colon)
                              <*> (C.indented *> parseValue)

parseAbs :: P.Parsec String ParseState Value
parseAbs = do
  C.lexeme $ P.char '\\'
  uncurriedAbs <|> curriedAbs
  where
  uncurriedAbs :: P.Parsec String ParseState Value
  uncurriedAbs = do
    args <- C.indented *> C.parens ((C.indented *> C.parseIdent) `P.sepBy` (C.indented *> C.comma))
    C.lexeme $ C.indented *> P.string "->"
    value <- parseValue
    return $ Abs args value
  curriedAbs :: P.Parsec String ParseState Value
  curriedAbs = do
    args <- P.many1 (C.indented *> C.parseIdent)
    C.lexeme $ C.indented *> P.string "->"
    value <- parseValue
    return $ foldl (\ret arg -> Abs [arg] ret) value args

parseApp :: P.Parsec String ParseState Value
parseApp = App <$> parseValue
               <*> (C.indented *> C.parens (parseValue `P.sepBy` (C.indented *> C.comma)))

parseVar :: P.Parsec String ParseState Value
parseVar = Var <$> C.parseIdent

parseConstructor :: P.Parsec String ParseState Value
parseConstructor = Constructor <$> C.properName

parseCase :: P.Parsec String ParseState Value
parseCase = Case <$> P.between (P.try (C.reserved "case")) (C.indented *> C.reserved "of") parseValue
                 <*> (C.indented *> C.mark (P.many (C.same *> C.mark parseCaseAlternative)))

parseCaseAlternative :: P.Parsec String ParseState (Binder, Value)
parseCaseAlternative = (,) <$> (parseGuardedBinder <* C.lexeme (P.string "->"))
                           <*> parseValue
                           P.<?> "case alternative"

parseIfThenElse :: P.Parsec String ParseState Value
parseIfThenElse = IfThenElse <$> (P.try (C.reserved "if") *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "then" *> C.indented *> parseValue)
                             <*> (C.indented *> C.reserved "else" *> C.indented *> parseValue)

parseBlock :: P.Parsec String ParseState Value
parseBlock = Block <$> (P.try (C.reserved "do") *> parseManyStatements)

parseManyStatements :: P.Parsec String ParseState [Statement]
parseManyStatements = C.indented *> C.mark (P.many (C.same *> C.mark parseStatement)) P.<?> "block"

parseValueAtom :: P.Parsec String ParseState Value
parseValueAtom = C.indented *> P.choice
            [ P.try parseNumericLiteral
            , P.try parseStringLiteral
            , P.try parseBooleanLiteral
            , parseArrayLiteral
            , parseObjectLiteral
            , parseAbs
            , P.try parseVar
            , P.try parseConstructor
            , parseBlock
            , parseCase
            , parseIfThenElse
            , C.parens parseValue ]

parsePropertyUpdate :: P.Parsec String ParseState (String, Value)
parsePropertyUpdate = do
  name <- C.lexeme C.identifier
  C.lexeme $ C.indented *> P.char '='
  value <- C.indented *> parseValue
  return (name, value)

parseValue :: P.Parsec String ParseState Value
parseValue = do
  customOps <- fixities <$> P.getState
  (buildExpressionParser (operators customOps)
   . C.buildPostfixParser postfixTable2
   $ indexersAndAccessors) P.<?> "expression"
  where
  indexersAndAccessors = C.buildPostfixParser postfixTable1 parseValueAtom
  postfixTable1 = [ Accessor <$> (C.indented *> C.dot *> C.indented *> C.identifier)
                  , P.try $ flip ObjectUpdate <$> (C.indented *> C.braces ((C.indented *> parsePropertyUpdate) `P.sepBy1` (C.indented *> C.comma))) ]
  postfixTable2 = [ P.try (C.indented *> indexersAndAccessors >>= \t2 -> return (\t1 -> App t1 [t2]))
                  , P.try $ flip App <$> (C.indented *> C.parens (parseValue `P.sepBy` (C.indented *> C.comma)))
                  , flip TypedValue <$> (P.try (C.lexeme (C.indented *> P.string "::")) *> parsePolyType) ]
  operators user =
              [ [ Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "!") >> return (Unary Not)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "~") >> return (Unary BitwiseNot)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "-") >> return (Unary Negate)
                , Prefix $ C.lexeme (P.try $ C.indented *> C.reservedOp "+") >> return id ]
              ] ++ customOperatorTable user ++
              [ [ Infix (C.lexeme (P.try (C.indented *> C.parseIdentInfix P.<?> "operator") >>= \ident -> return $ \t1 t2 -> App (App (Var ident) [t1]) [t2])) AssocLeft ]
              , [ Infix (C.lexeme (P.try $ C.indented *> C.reservedOp "!!") >> return (flip Indexer)) AssocRight ]
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

customOperatorTable :: M.Map String Fixity -> OperatorTable String ParseState Identity Value
customOperatorTable fixities =
  let
    ops = map (\(name, Fixity a p) -> (name, (a, p))) . M.toList $ fixities
    sorted = sortBy (compare `on` (snd . snd)) ops
    levels = groupBy ((==) `on` (snd . snd)) sorted
  in
    map (map $ \(name, (a, _)) ->
      flip Infix (toAssoc a) $
        C.lexeme $ P.try $ do
          C.indented
          C.reservedOp name P.<?> "operator"
          return $ \t1 t2 -> App (App (Var (Op name)) [t1]) [t2])
      levels

toAssoc :: Associativity -> Assoc
toAssoc Infixl = AssocLeft
toAssoc Infixr = AssocRight

parseVariableIntroduction :: P.Parsec String ParseState Statement
parseVariableIntroduction = do
  C.reserved "var"
  name <- C.indented *> C.parseIdent
  C.lexeme $ C.indented *> P.char '='
  value <- parseValue
  return $ VariableIntroduction name value

parseAssignment :: P.Parsec String ParseState Statement
parseAssignment = do
  tgt <- C.parseIdent
  C.lexeme $ C.indented *> P.char '='
  value <- parseValue
  return $ Assignment tgt value

parseWhile :: P.Parsec String ParseState Statement
parseWhile = While <$> (C.reserved "while" *> C.indented *> parseValue <* C.indented <* C.colon)
                   <*> parseManyStatements

parseFor :: P.Parsec String ParseState Statement
parseFor = For <$> (C.reserved "for" *> C.indented *> C.parseIdent)
               <*> (C.indented *> C.lexeme (P.string "<-") *> parseValue)
               <*> (C.indented *> C.reserved "until" *> parseValue <* C.colon)
               <*> parseManyStatements

parseForEach :: P.Parsec String ParseState Statement
parseForEach = ForEach <$> (C.reserved "foreach" *> C.indented *> C.parseIdent)
                       <*> (C.indented *> C.reserved "in" *> parseValue <* C.colon)
                       <*> parseManyStatements

parseIf :: P.Parsec String ParseState Statement
parseIf = If <$> parseIfStatement

parseIfStatement :: P.Parsec String ParseState IfStatement
parseIfStatement =
  IfStatement <$> (C.reserved "if" *> C.indented *> parseValue <* C.indented <* C.colon)
              <*> parseManyStatements
              <*> P.optionMaybe (C.same *> parseElseStatement)

parseElseStatement :: P.Parsec String ParseState ElseStatement
parseElseStatement = C.reserved "else" >> (ElseIf <$> (C.indented *> parseIfStatement)
                                           <|> Else <$> (C.indented *> C.colon *> parseManyStatements))

parseReturn :: P.Parsec String ParseState Statement
parseReturn = Return <$> (C.reserved "return" *> parseValue)

parseStatement :: P.Parsec String ParseState Statement
parseStatement = P.choice (map P.try
                 [ parseVariableIntroduction
                 , parseAssignment
                 , parseWhile
                 , parseFor
                 , parseForEach
                 , parseIf
                 , parseReturn ]) P.<?> "statement"

parseStringBinder :: P.Parsec String ParseState Binder
parseStringBinder = StringBinder <$> C.stringLiteral

parseBooleanBinder :: P.Parsec String ParseState Binder
parseBooleanBinder = BooleanBinder <$> booleanLiteral

parseNumberBinder :: P.Parsec String ParseState Binder
parseNumberBinder = NumberBinder <$> C.integerOrFloat

parseVarBinder :: P.Parsec String ParseState Binder
parseVarBinder = VarBinder <$> C.parseIdent

parseNullaryBinder :: P.Parsec String ParseState Binder
parseNullaryBinder = NullaryBinder <$> C.lexeme C.properName

parseUnaryBinder :: P.Parsec String ParseState Binder
parseUnaryBinder = UnaryBinder <$> C.lexeme C.properName <*> (C.indented *> parseBinder)

parseObjectBinder :: P.Parsec String ParseState Binder
parseObjectBinder = ObjectBinder <$> C.braces ((C.indented *> parseIdentifierAndBinder) `P.sepBy` (C.indented *> C.comma))

parseArrayBinder :: P.Parsec String ParseState Binder
parseArrayBinder = C.squares $ ArrayBinder <$> ((C.indented *> parseBinder) `P.sepBy` (C.indented *> C.comma))
                                           <*> P.optionMaybe (C.indented *> C.colon *> C.indented *> parseBinder)

parseNamedBinder :: P.Parsec String ParseState Binder
parseNamedBinder = NamedBinder <$> (C.parseIdent <* C.indented <* C.lexeme (P.char '@'))
                               <*> (C.indented *> parseBinder)

parseNullBinder :: P.Parsec String ParseState Binder
parseNullBinder = C.lexeme (P.char '_') *> P.notFollowedBy C.identLetter *> return NullBinder

parseIdentifierAndBinder :: P.Parsec String ParseState (String, Binder)
parseIdentifierAndBinder = do
  name <- C.lexeme C.identifier
  C.lexeme $ C.indented *> P.char '='
  binder <- C.indented *> parseBinder
  return (name, binder)

parseBinder :: P.Parsec String ParseState Binder
parseBinder = P.choice (map P.try
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

parseGuardedBinder :: P.Parsec String ParseState Binder
parseGuardedBinder = flip ($) <$> parseBinder <*> P.option id (GuardedBinder <$> (C.indented *> C.lexeme (P.char '|') *> C.indented *> parseValue))
