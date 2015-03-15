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

module Parser
  ( parseCommand
  ) where

import Prelude hiding (lex)

import qualified Commands as C
import qualified Directive as D

import Data.Char (isSpace)

import Control.Applicative hiding (many)

import Text.Parsec hiding ((<|>))

import qualified Language.PureScript as P
import qualified Language.PureScript.Parser.Common as C (mark, same)

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either String C.Command
parseCommand cmdString =
  case cmdString of
    (':' : cmd) -> parseDirective cmd
    _ -> parseRest psciCommand cmdString

parseRest :: P.TokenParser a -> String -> Either String a
parseRest p s = either (Left . show) Right $ do
  ts <- P.lex "" s
  P.runTokenParser "" (p <* eof) ts

psciCommand :: P.TokenParser C.Command
psciCommand = choice (map try parsers)
  where
  parsers =
    [ psciLet
    , psciImport
    , psciOtherDeclaration
    , psciExpression
    ]

trim :: String -> String
trim = trimEnd . trimStart

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse

parseDirective :: String -> Either String C.Command
parseDirective cmd =
  case D.parseDirective dstr of
    Just D.Help -> return C.Help
    Just D.Quit -> return C.Quit
    Just D.Reset -> return C.Reset
    Just D.Browse -> C.Browse <$> parseRest P.moduleName arg
    Just D.Load -> return $ C.LoadFile (trim arg)
    Just D.Show -> return $ C.Show (trim arg)
    Just D.Type -> C.TypeOf <$> parseRest P.parseValue arg
    Just D.Kind -> C.KindOf <$> parseRest P.parseType arg
    Nothing -> Left $ "Unrecognized command. Type :? for help."
  where (dstr, arg) = break isSpace cmd

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpression :: P.TokenParser C.Command
psciExpression = C.Expression <$> P.parseValue

-- |
-- PSCI version of @let@.
-- This is essentially let from do-notation.
-- However, since we don't support the @Eff@ monad,
-- we actually want the normal @let@.
--
psciLet :: P.TokenParser C.Command
psciLet = C.Decls <$> (P.reserved "let" *> P.indented *> manyDecls)
  where
  manyDecls :: P.TokenParser [P.Declaration]
  manyDecls = C.mark (many1 (C.same *> P.parseLocalDeclaration))

-- | Imports must be handled separately from other declarations, so that
-- :show import works, for example.
psciImport :: P.TokenParser C.Command
psciImport = C.Import <$> P.parseImportDeclaration'

-- | Any other declaration that we don't need a 'special case' parser for
-- (like let or import declarations).
psciOtherDeclaration :: P.TokenParser C.Command
psciOtherDeclaration = C.Decls . (:[]) <$> do
  decl <- discardPositionInfo <$> P.parseDeclaration
  if acceptable decl
    then return decl
    else fail "this kind of declaration is not supported in psci"

discardPositionInfo :: P.Declaration -> P.Declaration
discardPositionInfo (P.PositionedDeclaration _ _ d) = d
discardPositionInfo d = d

acceptable :: P.Declaration -> Bool
acceptable (P.DataDeclaration _ _ _ _) = True
acceptable (P.TypeSynonymDeclaration _ _ _) = True
acceptable (P.ExternDeclaration _ _ _ _) = True
acceptable (P.ExternDataDeclaration _ _) = True
acceptable (P.ExternInstanceDeclaration _ _ _ _) = True
acceptable (P.TypeClassDeclaration _ _ _ _) = True
acceptable (P.TypeInstanceDeclaration _ _ _ _ _) = True
acceptable _ = False
