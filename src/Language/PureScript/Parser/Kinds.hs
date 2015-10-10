-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Kinds
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- A parser for kinds
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

module Language.PureScript.Parser.Kinds (
    parseKind
) where

import Language.PureScript.Kinds
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseStar :: TokenParser Kind
parseStar = const Star <$> symbol' "*"

parseBang :: TokenParser Kind
parseBang = const Bang <$> symbol' "!"

parseTypeAtom :: TokenParser Kind
parseTypeAtom = indented *> P.choice (map P.try
            [ parseStar
            , parseBang
            , parens parseKind ])
-- |
-- Parse a kind
--
parseKind :: TokenParser Kind
parseKind = P.buildExpressionParser operators parseTypeAtom P.<?> "kind"
  where
  operators = [ [ P.Prefix (symbol' "#" >> return Row) ]
              , [ P.Infix (P.try rarrow >> return FunKind) P.AssocRight ] ]
