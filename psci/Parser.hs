-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  (c) Phil Freeman 2014
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parser for PSCI.
--
-----------------------------------------------------------------------------

module Parser where

import Control.Applicative

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P

-- |
-- PSCI version of modules
--
psciModules :: Parsec String P.ParseState [P.Module]
psciModules = P.parseModules

-- |
-- PSCI version of 'let'.
-- This is essentially let from do-notation.
-- However, since we don't support the 'Eff' monad, we actually want the normal 'let'.
--
psciLet :: Parsec String P.ParseState (P.Value -> P.Value)
psciLet = P.Let <$> (P.reserved "let" *> P.indented *> P.parseBinder)
                <*> (P.indented *> P.reservedOp "=" *> P.parseValue)

-- |
-- PSCI version of any other valid expression.
--
psciExpression :: Parsec String P.ParseState P.Value
psciExpression = P.whiteSpace *> P.parseValue <* eof

-- |
-- Parser for PSCI.
-- This is really just a wrapper around 'Language.PureScript.runIndentParser'
--
psciParser :: Parsec String P.ParseState a -> String -> Either ParseError a
psciParser = P.runIndentParser ""

-- |
-- Parses PSCI modules.
--
parseModules :: String -> Either ParseError [P.Module]
parseModules = psciParser psciModules

-- |
-- Parses PSCI 'let' bindings.
--
parseLet :: String -> Either ParseError (P.Value -> P.Value)
parseLet = psciParser psciLet

-- |
-- Parses PSCI expressions.
--
parseExpression :: String -> Either ParseError P.Value
parseExpression = psciParser psciExpression
