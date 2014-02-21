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

import Commands

import Control.Applicative hiding (many)

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

parseCommands :: String -> Either ParseError Command
parseCommands = psciParser $ choice availableCommands

psciHelp :: Parsec String P.ParseState Command
psciHelp = Help <$ (string ":?" *> spaces)

psciImport :: Parsec String P.ParseState Command
psciImport = Import <$> (string ":i" *> spaces *> P.properName)

psciLoadFile :: Parsec String P.ParseState Command
psciLoadFile = LoadFile <$> do
  string ":m"
  spaces
  try (manyTill anyChar space) <|> many1 anyChar

psciQuit :: Parsec String P.ParseState Command
psciQuit = Quit <$ (string ":q" *> spaces)

psciReload :: Parsec String P.ParseState Command
psciReload = Reload <$ (string ":r" *> spaces)

psciTypeOf :: Parsec String P.ParseState Command
psciTypeOf = TypeOf <$> (string ":t" *> spaces *> many anyChar)

availableCommands :: [Parsec String P.ParseState Command]
availableCommands =
  map try [ psciHelp
          , psciImport
          , psciLoadFile
          , psciQuit
          , psciReload
          , psciTypeOf
          ]
