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

import Control.Applicative hiding (many)

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P

-- |
-- PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad,
-- we actually want the normal @let@.
--
psciLet :: P.TokenParser Command
psciLet = Let <$> (P.Let <$> (P.reserved "let" *> P.braces (P.semiSep P.parseDeclaration)))

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either ParseError Command
parseCommand s = do
  ts <- P.lex "" s
  flip (P.runTokenParser "") ts $ choice
    [ P.colon *> choice [ psciHelp
                        , psciImport
                        , psciLoadFile
                        , psciQuit
                        , psciReload
                        , psciTypeOf
                        , psciKindOf
                        , psciBrowse
                        , psciShowModules
                        ]
    , try psciLet
    , psciExpression
    ] <* eof

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpression :: P.TokenParser Command
psciExpression = Expression <$> P.parseValue

-- |
-- Parses 'Commands.Help' command.
--
psciHelp :: P.TokenParser Command
psciHelp = Help <$ P.symbol' "?"

-- |
-- Parses 'Commands.Import' command.
--
psciImport :: P.TokenParser Command
psciImport = Import <$> (P.reserved "i" *> P.moduleName)

-- |
-- Parses 'Commands.LoadFile' command.
--
psciLoadFile :: P.TokenParser Command
psciLoadFile = fail "Not implemented" {-LoadFile . trimEnd <$> (reserved "m" *> P.lname)

-- | Trim end of input string
trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse -}

-- |
-- Parses 'Commands.Quit' command.
--
psciQuit :: P.TokenParser Command
psciQuit = Quit <$ P.reserved "q"

-- |
-- Parses 'Commands.Reload' command.
--
psciReload :: P.TokenParser Command
psciReload = Reset <$ P.reserved "r"

-- |
-- Parses 'Commands.TypeOf' command.
--
psciTypeOf :: P.TokenParser Command
psciTypeOf = TypeOf <$> (P.reserved "t" *> P.parseValue)


-- |
-- Parses 'Commands.KindOf' command.
--
psciKindOf :: P.TokenParser Command
psciKindOf = KindOf <$> (P.reserved "k" *> P.parseType)

-- |
-- Parses 'Commands.Browse' command.
--
psciBrowse :: P.TokenParser Command
psciBrowse = Browse <$> (P.reserved "b" *> P.moduleName)

-- |
-- Show Command
psciShowModules :: P.TokenParser Command
psciShowModules = Show <$> (P.reserved "s" *> P.lname)
