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
import Language.PureScript.Kinds
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Kinds
import Language.PureScript.Parser.Lexer
import Language.PureScript.Types
import Language.PureScript.Label (Label(..))

import qualified Text.Parsec as P
import qualified Text.Parsec.Expr as P

parseFunction :: TokenParser SourceType
parseFunction = parens rarrow *> return tyFunction

parseObject :: TokenParser SourceType
parseObject = withSourceAnnF $ braces $ do
  rows <- parseRow
  return $ \ann -> TypeApp ann tyRecord rows

parseTypeLevelString :: TokenParser SourceType
parseTypeLevelString = withSourceAnnF $ flip TypeLevelString <$> stringLiteral

parseTypeWildcard :: TokenParser SourceType
parseTypeWildcard = withSourceAnnF $ do
  name <- Just <$> holeLit
      <|> Nothing <$ underscore
  return $ flip TypeWildcard name

parseTypeVariable :: TokenParser SourceType
parseTypeVariable = withSourceAnnF $ do
  ident <- identifier
  when (ident `elem` reservedTypeNames) $ P.unexpected (T.unpack ident)
  return $ \ann -> TypeVar ann ident

parseTypeConstructor :: TokenParser SourceType
parseTypeConstructor = withSourceAnnF $ flip TypeConstructor <$> parseQualified typeName

parseForAll :: TokenParser SourceType
parseForAll =
  mkForAll
    <$> ((reserved "forall" <|> reserved "∀")
          *> (P.many1 $ indented *> (withSourceAnnF $ flip (,) <$> identifier))
          <* indented <* dot)
    <*> parseType

-- |
-- Parse an atomic type with no `forall`
--
noForAll :: TokenParser SourceType -> TokenParser SourceType
noForAll p = do
 ty <- p
 when (containsForAll ty) $ P.unexpected "forall"
 return ty

-- |
-- Parse a type as it appears in e.g. a data constructor
--
parseTypeAtom :: TokenParser SourceType
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
            , parseParensInType
            ]

parseParensInType :: TokenParser SourceType
parseParensInType = withSourceAnnF $ flip ParensInType <$> parens parsePolyType

parseConstrainedType :: TokenParser (SourceAnn, [SourceConstraint], SourceType)
parseConstrainedType = withSourceAnnF $ do
  constraints <- parens (commaSep1 parseConstraint) <|> pure <$> parseConstraint
  _ <- rfatArrow
  indented
  ty <- parseType
  return (, constraints, ty)
  where
  parseConstraint = withSourceAnnF $ do
    className <- parseQualified properName
    indented
    ty <- P.many parseTypeAtom
    return $ \ann -> Constraint ann className ty Nothing

-- This is here to improve the error message when the user
-- tries to use the old style constraint contexts.
-- TODO: Remove this before 1.0
typeOrConstrainedType :: TokenParser SourceType
typeOrConstrainedType = do
  e <- P.try (Left <$> parseConstrainedType) <|> Right <$> parseTypeAtom
  case e of
    Left (ann, [c], ty) -> pure (ConstrainedType ann c ty)
    Left _ ->
      P.unexpected $
        unlines [ "comma in constraints."
                , ""
                , "Class constraints in type annotations can no longer be grouped in parentheses."
                , "Each constraint should now be separated by `=>`, for example:"
                , "    `(Applicative f, Semigroup a) => a -> f a -> f a`"
                , "  would now be written as:"
                , "    `Applicative f => Semigroup a => a -> f a -> f a`."
                ]
    Right ty -> pure ty

parseAnyType :: TokenParser SourceType
parseAnyType = P.buildExpressionParser operators (buildPostfixParser postfixTable typeOrConstrainedType) P.<?> "type"
  where
  operators = [ [ P.Infix (return mkTypeApp) P.AssocLeft ]
              , [ P.Infix parseTypeOp P.AssocRight
                ]
              , [ P.Infix (rarrow $> function) P.AssocRight ]
              ]
  postfixTable = [ parseKindedType
                 ]

  mkTypeApp lhs rhs =
    TypeApp (widenSourceAnn (getAnnForType lhs) (getAnnForType rhs)) lhs rhs

  parseTypeOp = withSourceAnnF $ do
    ident <- P.try (parseQualified parseOperator)
    return $ \ann lhs rhs ->
      BinaryNoParensType (widenSourceAnn (getAnnForType lhs) (getAnnForType rhs)) (TypeOp ann ident) lhs rhs

  parseKindedType ty = do
    kind <- indented *> doubleColon *> parseKind
    return $ KindedType (widenSourceAnn (getAnnForType ty) (getAnnForKind kind)) ty kind


-- |
-- Parse a monotype
--
parseType :: TokenParser SourceType
parseType = do
  ty <- parseAnyType
  unless (isMonoType ty) $ P.unexpected "polymorphic type"
  return ty

-- |
-- Parse a polytype
--
parsePolyType :: TokenParser SourceType
parsePolyType = parseAnyType

-- |
-- Parse an atomic type with no wildcards
--
noWildcards :: TokenParser SourceType -> TokenParser SourceType
noWildcards p = do
  ty <- p
  when (containsWildcards ty) $ P.unexpected "type wildcard"
  return ty

parseRowListItem :: TokenParser SourceType -> TokenParser (RowListItem SourceAnn)
parseRowListItem p = withSourceAnnF $
  (\name ty ann -> RowListItem ann name ty)
    <$> (indented *> (Label <$> parseLabel) <* indented <* doubleColon) <*> p

parseRowEnding :: TokenParser SourceType
parseRowEnding =
  (indented *> pipe *> indented *> parseType)
    <|> withSourceAnnF (return REmpty)

parseRow :: TokenParser SourceType
parseRow = (curry rowFromList <$> commaSep (parseRowListItem parsePolyType) <*> parseRowEnding) P.<?> "row"
