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
import Data.List (isPrefixOf)

import Control.Applicative hiding (many)

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P
import qualified Language.PureScript.Parser.Common as C (mark, same)

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either String Command
parseCommand cmdString =
  case splitCommand cmdString of
    Just (cmd, arg)
      | matches "?" -> return Help
      | matches "help" -> return Help
      | matches "quit" -> return Quit
      | matches "reset" -> return Reset
      | matches "import" -> Import <$> parseRest P.moduleName arg
      | matches "browse" -> Browse <$> parseRest P.moduleName arg
      | matches "module" -> return $ LoadFile (trimEnd arg)
      | matches "show" -> return $ Show (trimEnd cmd)
      | matches "type" -> TypeOf <$> parseRest P.parseValue arg
      | matches "kind" -> KindOf <$> parseRest P.parseType arg
      | otherwise -> Left $ "Unrecognized command. Type :? for help."
      where
      matches = isPrefixOf cmd
    Nothing -> parseRest (psciLet <|> psciExpression) cmdString
  where
  parseRest :: P.TokenParser a -> String -> Either String a
  parseRest p s = either (Left . show) Right $ do
    ts <- P.lex "" s
    P.runTokenParser "" (p <* eof) ts

  trimStart :: String -> String
  trimStart = dropWhile isSpace

  trimEnd :: String -> String
  trimEnd = reverse . trimStart . reverse

  -- |
  -- Tries to split a command into a directive and the argument.
  --
  splitCommand :: String -> Maybe (String, String)
  splitCommand (':' : cmd) = Just (directive, trimStart arg)
    where
    (directive, arg) = break isSpace cmd
  splitCommand _ = Nothing

  -- |
  -- Parses expressions entered at the PSCI repl.
  --
  psciExpression :: P.TokenParser Command
  psciExpression = Expression <$> P.parseValue

  -- |
  -- PSCI version of @let@.
  -- This is essentially let from do-notation.
  -- However, since we don't support the @Eff@ monad,
  -- we actually want the normal @let@.
  --
  psciLet :: P.TokenParser Command
  psciLet = Let <$> (P.reserved "let" *> P.indented *> manyDecls)
    where
    manyDecls :: P.TokenParser [P.Declaration]
    manyDecls = C.mark (many1 (C.same *> P.parseDeclaration))
