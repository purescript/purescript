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

import Prelude hiding (lex)

import Commands

import Data.Char (isSpace)

import Control.Applicative hiding (many)

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P
import qualified Language.PureScript.Parser.Common as C (mark, same)

parseRestWith :: P.TokenParser a -> Parsec String () a
parseRestWith p = do
  s <- many anyChar
  case P.lex "" s >>= P.runTokenParser "" p of
    Left err -> fail (show err)
    Right a -> return a

-- |
-- PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad,
-- we actually want the normal @let@.
--
psciLet :: Parsec String () Command
psciLet = Let <$> (P.Let <$> (string "let" *> spaces *> parseRestWith manyDecls))
  where
  manyDecls :: P.TokenParser [P.Declaration]
  manyDecls = C.mark (many1 (C.same *> P.parseDeclaration))

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either ParseError Command
parseCommand = flip parse "" $ choice
                    [ spaces *> char ':' *> (psciHelp <|> psciImport <|> psciLoadFile <|> psciQuit <|> psciReload <|> psciTypeOf <|> psciKindOf <|> psciBrowse <|> psciShowModules)
                    , try psciLet
                    , psciExpression
                    ] <* eof

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpression :: Parsec String () Command
psciExpression = Expression <$> parseRestWith P.parseValue

-- |
-- Parses 'Commands.Help' command.
--
psciHelp :: Parsec String () Command
psciHelp = Help <$ char '?'

-- |
-- Parses 'Commands.Import' command.
--
psciImport :: Parsec String () Command
psciImport = Import <$> (char 'i' *> spaces *> parseRestWith P.moduleName)

-- |
-- Parses 'Commands.LoadFile' command.
--
psciLoadFile :: Parsec String () Command
psciLoadFile = LoadFile . trimEnd <$> (char 'm' *> spaces *> manyTill anyChar eof)

-- | Trim end of input string
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse

-- |
-- Parses 'Commands.Quit' command.
--
psciQuit :: Parsec String () Command
psciQuit = Quit <$ char 'q'

-- |
-- Parses 'Commands.Reload' command.
--
psciReload :: Parsec String () Command
psciReload = Reset <$ char 'r'

-- |
-- Parses 'Commands.TypeOf' command.
--
psciTypeOf :: Parsec String () Command
psciTypeOf = TypeOf <$> (char 't' *> spaces *> parseRestWith P.parseValue)


-- |
-- Parses 'Commands.KindOf' command.
--
psciKindOf :: Parsec String () Command
psciKindOf = KindOf <$> (char 'k' *> spaces *> parseRestWith P.parseType)

-- |
-- Parses 'Commands.Browse' command.
--
psciBrowse :: Parsec String () Command
psciBrowse = Browse <$> (char 'b' *> spaces *> parseRestWith P.moduleName)

-- |
-- Show Command
psciShowModules :: Parsec String () Command
psciShowModules = Show . trimEnd <$> (char 's' *> spaces *> manyTill anyChar eof)
