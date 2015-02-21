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

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either String Command
parseCommand cmdString =
  case splitCommand cmdString of
    Just ('?', _) -> return Help
    Just ('q', _) -> return Quit
    Just ('r', _) -> return Reset
    Just ('i', moduleName) -> Import <$> parseRest P.moduleName moduleName
    Just ('b', moduleName) -> Browse <$> parseRest P.moduleName moduleName
    Just ('m', filename) -> return $ LoadFile (trimEnd filename)
    Just ('s', command) -> return $ Show (trimEnd command)
    Just ('t', expr) -> TypeOf <$> parseRest P.parseValue expr
    Just ('k', ty) -> KindOf <$> parseRest P.parseType ty
    Just _ -> Left $ "Unrecognized command. Type :? for help."
    Nothing -> parseRest (psciLet <|> psciExpression) cmdString
  where
  parseRest :: P.TokenParser a -> String -> Either String a
  parseRest p s = either (Left . show) Right $ do
    ts <- P.lex "" s
    P.runTokenParser "" (p <* eof) ts

  trimEnd :: String -> String
  trimEnd = reverse . dropWhile isSpace . reverse

  -- |
  -- Split a command into a command char and the trailing string
  --
  splitCommand :: String -> Maybe (Char, String)
  splitCommand (':' : c : s) = Just (c, dropWhile isSpace s)
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
