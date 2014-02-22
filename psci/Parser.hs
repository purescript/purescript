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

import Data.Char

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P

-- |
-- PSCI version of modules
--
psciModules :: Parsec String P.ParseState [P.Module]
psciModules = P.parseModules

-- |
-- PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad,
-- we actually want the normal @let@.
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
-- Parses PSCI @let@ bindings.
--
parseLet :: String -> Either ParseError (P.Value -> P.Value)
parseLet = psciParser psciLet

-- |
-- Parses PSCI @expressions@.
--
parseExpression :: String -> Either ParseError P.Value
parseExpression = psciParser psciExpression

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommands :: String -> Either ParseError Command
parseCommands = psciParser $ choice (map try availableCommands)

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpr :: Parsec String P.ParseState Command
psciExpr = Expression <$> (lookAhead (try (noneOf ":")) *> many1 anyChar)

-- |
-- Parses 'Commands.Help' command.
--
psciHelp :: Parsec String P.ParseState Command
psciHelp = Help <$ (spaces *> string ":?" *> spaces)

-- |
-- Parses 'Commands.Import' command.
--
psciImport :: Parsec String P.ParseState Command
psciImport = Import <$> (spaces *> string ":i" *> spaces *> P.moduleName)

-- |
-- Parses 'Commands.LoadFile' command.
--
psciLoadFile :: Parsec String P.ParseState Command
psciLoadFile = LoadFile <$> do
  spaces
  string ":m"
  spaces
  rawPath <- manyTill anyChar eof
  return . reverse . dropWhile isSpace . reverse $ rawPath

-- |
-- Parses 'Commands.Quit' command.
--
psciQuit :: Parsec String P.ParseState Command
psciQuit = Quit <$ (spaces *> string ":q" *> spaces)

-- |
-- Parses 'Commands.Reload' command.
--
psciReload :: Parsec String P.ParseState Command
psciReload = Reload <$ (spaces *> string ":r" *> spaces)

-- |
-- Parses 'Commands.TypeOf' command.
--
psciTypeOf :: Parsec String P.ParseState Command
psciTypeOf = TypeOf <$> (spaces *> string ":t" *> spaces *> many anyChar)

-- |
-- List of commands that are available from PSCI.
--
availableCommands :: [Parsec String P.ParseState Command]
availableCommands = [ psciExpr
                    , psciHelp
                    , psciImport
                    , psciLoadFile
                    , psciQuit
                    , psciReload
                    , psciTypeOf
                    ]
