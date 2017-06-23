module Language.PureScript.Parser.Types
  ( parseType
  , parsePolyType
  , noForAll
  , noWildcards
  , parseTypeAtom
  ) where

import Prelude.Compat

import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.Environment
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseFunction :: TokenParser Type
parseFunction = parens rarrow *> return tyFunction

parseObject :: TokenParser Type
parseObject = braces $ TypeApp tyRecord <$> parseRow

parseTypeLevelString :: TokenParser Type
parseTypeLevelString = TypeLevelString <$> stringLiteral

parseTypeWildcard :: TokenParser Type
parseTypeWildcard = do
  start <- P.getPosition
  let end = P.incSourceColumn start 1
  underscore
  return $ TypeWildcard (SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos end))

parseTypeVariable :: TokenParser Type
parseTypeVariable = do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected (T.unpack ident)
  return $ TypeVar ident

parseTypeConstructor :: TokenParser Type
parseTypeConstructor = TypeConstructor <$> parseQualified typeName

parseForAll :: TokenParser Type
parseForAll = mkForAll <$> ((reserved "forall" <|> reserved "∀") *> P.many1 (indented *> identifier) <* indented <* dot)
                       <*> parseType


-- |
-- Parse an atomic type with no `forall`
--
noForAll :: TokenParser Type -> TokenParser Type
noForAll p = do
 ty <- p
 when (containsForAll ty) $ P.unexpected "forall"
 return ty

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: TokenParser Type
parseTypeAtom = indented *> P.choice
            [ P.try parseFunction
            , parseTypeLevelString
            , parseObject
            , parseTypeWildcard
            , parseForAll
            , parseTypeVariable
            , parseTypeConstructor
            -- This try is needed due to some unfortunate ambiguities between rows and kinded types
            , P.try (parens parseRow)
            , ParensInType <$> parens parsePolyType
            ]

parseAnyType :: TokenParser Type
parseAnyType = P.buildExpressionParser operators (buildPostfixParser postfixTable parseTypeAtom) P.<?> "type"
  where
  operators = [ [ P.Infix (return TypeApp) P.AssocLeft ]
              , [ P.Infix (P.try (parseQualified parseOperator) >>= \ident ->
                    return (BinaryNoParensType (TypeOp ident))) P.AssocRight
                ]
              , [ P.Infix (rfatArrow $> constrained) P.AssocRight ]
              , [ P.Infix (rarrow $> function) P.AssocRight ]
              ]
  postfixTable = [ \t -> KindedType t <$> (indented *> doubleColon *> parseKind)
                 ]

constrained :: Type -> Type -> Type
constrained = ConstrainedType . toConstraint [] where
  toConstraint :: [Type] -> Type -> Constraint
  toConstraint args (TypeApp con arg) = toConstraint (arg : args) con
  toConstraint args con = Constraint con args Nothing

-- |
-- Parse a monotype
--
parseType :: TokenParser Type
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: TokenParser Type
parsePolyType = parseAnyType

-- |
-- Parse an atomic type with no wildcards
--
noWildcards :: TokenParser Type -> TokenParser Type
noWildcards p = do
  ty <- p
  when (containsWildcards ty) $ P.unexpected "type wildcard"
  return ty

parseNameAndType :: TokenParser t -> TokenParser (Label, t)
parseNameAndType p = (,) <$> (indented *> (Label <$> parseLabel) <* indented <* doubleColon) <*> p

parseRowEnding :: TokenParser Type
parseRowEnding = P.option REmpty $ indented *> pipe *> indented *> parseType

parseRow :: TokenParser Type
parseRow = (curry rowFromList <$> commaSep (parseNameAndType parsePolyType) <*> parseRowEnding) P.<?> "row"
