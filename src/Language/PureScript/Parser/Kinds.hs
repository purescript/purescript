-- |
-- A parser for kinds
--
module Language.PureScript.Parser.Kinds (parseKind) where

import Prelude.Compat

import Language.PureScript.Environment
import Language.PureScript.Kinds
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseStar :: TokenParser Kind
parseStar = const kindType <$> symbol' "*"

parseBang :: TokenParser Kind
parseBang = const kindEffect <$> symbol' "!"

parseNamedKind :: TokenParser Kind
parseNamedKind = NamedKind <$> parseQualified kindName

parseKindAtom :: TokenParser Kind
parseKindAtom = indented *> P.choice
            [ parseStar
            , parseBang
            , parseNamedKind
            , parens parseKind
            ]

-- |
-- Parse a kind
--
parseKind :: TokenParser Kind
parseKind = P.buildExpressionParser operators parseKindAtom P.<?> "kind"
  where
  operators = [ [ P.Prefix (symbol' "#" >> return Row) ]
              , [ P.Infix (rarrow >> return FunKind) P.AssocRight ] ]
