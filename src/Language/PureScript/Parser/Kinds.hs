-- |
-- A parser for kinds
--
module Language.PureScript.Parser.Kinds (parseKind) where

import Prelude.Compat

import Language.PureScript.Kinds
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseStar :: TokenParser Kind
parseStar = symbol' "*" *>
  P.parserFail "The `*` symbol is no longer used for the kind of types.\n  The new equivalent is the named kind `Type`."

parseBang :: TokenParser Kind
parseBang = symbol' "!" *>
  P.parserFail "The `!` symbol is no longer used for the kind of effects.\n  The new equivalent is the named kind `Effect`, defined in `Control.Monad.Eff` in the `purescript-eff` library."

parseNamedKind :: TokenParser Kind
parseNamedKind = NamedKind <$> parseQualified kindName

parseKindAtom :: TokenParser Kind
parseKindAtom =
  indented *> P.choice
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
