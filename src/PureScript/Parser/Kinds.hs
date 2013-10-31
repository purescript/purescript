-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Kinds
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript.Parser.Kinds (
    parseKind
) where

import PureScript.Kinds
import PureScript.Parser.State
import PureScript.Parser.Common
import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseStar :: P.Parsec String ParseState Kind
parseStar = const Star <$> lexeme (P.char '*')

parseRow :: P.Parsec String ParseState Kind
parseRow = const Row <$> lexeme (P.char '#')

parseTypeAtom :: P.Parsec String ParseState Kind
parseTypeAtom = indented *> P.choice (map P.try
            [ parseStar
            , parseRow
            , parens parseKind ])

parseKind :: P.Parsec String ParseState Kind
parseKind = P.buildExpressionParser operators parseTypeAtom P.<?> "kind"
  where
  operators = [ [ P.Infix (lexeme (P.try (P.string "->")) >> return FunKind) P.AssocRight ] ]
