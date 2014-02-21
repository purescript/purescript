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

import Text.Parsec

import qualified Language.PureScript as P

-- |
-- Parser for PSCI.
-- This is really just a wrapper around 'Language.PureScript.runIndentParser'
--
psciParser :: Parsec String P.ParseState a -> String -> Either ParseError a
psciParser = P.runIndentParser ""

-- |
-- Parses modules
--
parseModules :: String -> Either ParseError [P.Module]
parseModules = psciParser P.parseModules

-- |
-- Parser for our PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad, we actually want the normal @let@.
--
parseLet :: Parsec String P.ParseState (P.Value -> P.Value)
parseLet = P.Let <$> (P.reserved "let" *> P.indented *> P.parseBinder)
                 <*> (P.indented *> P.reservedOp "=" *> P.parseValue)

-- |
-- Parser for any other valid expression.
--
parseExpression :: Parsec String P.ParseState P.Value
parseExpression = P.whiteSpace *> P.parseValue <* eof
