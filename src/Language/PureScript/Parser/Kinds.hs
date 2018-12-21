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

parseNamedKind :: TokenParser SourceKind
parseNamedKind = withSourceAnnF $ do
  name <- parseQualified kindName
  return $ \ann -> NamedKind ann name

parseKindAtom :: TokenParser SourceKind
parseKindAtom =
  indented *> P.choice
    [ parseNamedKind
    , parens parseKind
    ]

-- |
-- Parse a kind
--
parseKind :: TokenParser SourceKind
parseKind = P.buildExpressionParser operators parseKindAtom P.<?> "kind"
  where
  operators = [ [ P.Prefix (withSourceAnnF $ symbol' "#" >> return Row) ]
              , [ P.Infix (withSourceAnnF $ rarrow >> return FunKind) P.AssocRight ] ]
