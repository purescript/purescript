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

module Parser (
    parseCommand
  ) where

import Commands

import Data.Char (isSpace)

import Control.Applicative hiding (many)

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P

-- |
-- PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad,
-- we actually want the normal @let@.
--
psciLet :: Parsec String P.ParseState Command
psciLet = Let <$> (P.Let <$> (P.reserved "let" *> P.indented *> P.parseBinder)
                         <*> (P.indented *> P.reservedOp "=" *> P.parseValue))

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either ParseError Command
parseCommand = P.runIndentParser "" $ choice
                    [ P.whiteSpace *> char ':' *> (psciHelp <|> psciImport <|> psciLoadFile <|> psciQuit <|> psciReload <|> psciTypeOf)
                    , try psciLet
                    , psciExpression
                    ] <* eof

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpression :: Parsec String P.ParseState Command
psciExpression = Expression <$> P.parseValue

-- |
-- Parses 'Commands.Help' command.
--
psciHelp :: Parsec String P.ParseState Command
psciHelp = Help <$ char '?'

-- |
-- Parses 'Commands.Import' command.
--
psciImport :: Parsec String P.ParseState Command
psciImport = Import <$> (char 'i' *> P.whiteSpace *> P.moduleName)

-- |
-- Parses 'Commands.LoadFile' command.
--
psciLoadFile :: Parsec String P.ParseState Command
psciLoadFile = LoadFile . trimEnd <$> (char 'm' *> P.whiteSpace *> manyTill anyChar eof)
  where
  trimEnd = reverse . dropWhile isSpace . reverse

-- |
-- Parses 'Commands.Quit' command.
--
psciQuit :: Parsec String P.ParseState Command
psciQuit = Quit <$ char 'q'

-- |
-- Parses 'Commands.Reload' command.
--
psciReload :: Parsec String P.ParseState Command
psciReload = Reload <$ char 'r'

-- |
-- Parses 'Commands.TypeOf' command.
--
psciTypeOf :: Parsec String P.ParseState Command
psciTypeOf = TypeOf <$> (char 't' *> P.whiteSpace *> P.parseValue)
