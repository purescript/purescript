module Language.PureScript.Sugar.Operators.Common where

import Prelude.Compat

import Control.Monad.State
import Control.Monad.Except

import Data.Either (rights)
import Data.Functor.Identity
import Data.List (sortOn)
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.List.NonEmpty as NEL
import qualified Data.Map as M

import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as P
import qualified Text.Parsec.Expr as P

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names

type Chain a = [Either a a]

type FromOp nameType a = a -> Maybe (SourceSpan, Qualified (OpName nameType))
type Reapply nameType a = SourceSpan -> Qualified (OpName nameType) -> a -> a -> a

toAssoc :: Associativity -> P.Assoc
toAssoc Infixl = P.AssocLeft
toAssoc Infixr = P.AssocRight
toAssoc Infix  = P.AssocNone

token :: (P.Stream s Identity t) => (t -> Maybe a) -> P.Parsec s u a
token = P.token (const "") (const (P.initialPos ""))

parseValue :: P.Parsec (Chain a) () a
parseValue = token (either Just (const Nothing)) P.<?> "expression"

parseOp
  :: FromOp nameType a
  -> P.Parsec (Chain a) () (SourceSpan, Qualified (OpName nameType))
parseOp fromOp = token (either (const Nothing) fromOp) P.<?> "operator"

matchOp
  :: FromOp nameType a
  -> Qualified (OpName nameType)
  -> P.Parsec (Chain a) () SourceSpan
matchOp fromOp op = do
  (ss, ident) <- parseOp fromOp
  guard $ ident == op
  pure ss

opTable
  :: [[(Qualified (OpName nameType), Associativity)]]
  -> FromOp nameType a
  -> Reapply nameType a
  -> [[P.Operator (Chain a) () Identity a]]
opTable ops fromOp reapply =
  map (map (\(name, a) -> P.Infix (P.try (matchOp fromOp name) >>= \ss -> return (reapply ss name)) (toAssoc a))) ops

matchOperators
  :: forall m a nameType
   . Show a
  => MonadError MultipleErrors m
  => (a -> Bool)
  -> (a -> Maybe (a, a, a))
  -> FromOp nameType a
  -> Reapply nameType a
  -> ([[P.Operator (Chain a) () Identity a]] -> P.OperatorTable (Chain a) () Identity a)
  -> [[(Qualified (OpName nameType), Associativity)]]
  -> a
  -> m a
matchOperators isBinOp extractOp fromOp reapply modOpTable ops = parseChains
  where
  parseChains :: a -> m a
  parseChains ty
    | True <- isBinOp ty = bracketChain (extendChain ty)
    | otherwise = pure ty
  extendChain :: a -> Chain a
  extendChain ty
    | Just (op, l, r) <- extractOp ty = Left l : Right op : extendChain r
    | otherwise = [Left ty]
  bracketChain :: Chain a -> m a
  bracketChain chain =
    case P.parse opParser "operator expression" chain of
      Right a -> pure a
      Left _ -> throwError . MultipleErrors $ mkErrors chain
  opParser :: P.Parsec (Chain a) () a
  opParser = P.buildExpressionParser (modOpTable (opTable ops fromOp reapply)) parseValue <* P.eof

  -- Generating a good error message involves a bit of work here, as the parser
  -- can't provide one for us.
  --
  -- We examine the expression chain, plucking out the operators and then
  -- grouping them by shared precedence, then if any of the following conditions
  -- are met, we have something to report:
  --   1. any of the groups have mixed associativity
  --   2. there is more than one occurrence of a non-associative operator in a
  --      precedence group
  mkErrors :: Chain a -> [ErrorMessage]
  mkErrors chain =
    let
      opInfo :: M.Map (Qualified (OpName nameType)) (Integer, Associativity)
      opInfo = M.fromList $ concatMap (\(n, o) -> map (\(name, assoc) -> (name, (n, assoc))) o) (zip [0..] ops)
      opPrec :: Qualified (OpName nameType) -> Integer
      opPrec = fromJust . fmap fst . flip M.lookup opInfo
      opAssoc :: Qualified (OpName nameType) -> Associativity
      opAssoc = fromJust . fmap snd . flip M.lookup opInfo
      chainOpSpans :: M.Map (Qualified (OpName nameType)) (NEL.NonEmpty SourceSpan)
      chainOpSpans = foldr (\(ss, name) -> M.alter (Just . maybe (pure ss) (NEL.cons ss)) name) M.empty . mapMaybe fromOp $ rights chain
      opUsages :: Qualified (OpName nameType) -> Int
      opUsages = maybe 0 NEL.length . flip M.lookup chainOpSpans
      precGrouped :: [NEL.NonEmpty (Qualified (OpName nameType))]
      precGrouped = NEL.groupWith opPrec . sortOn opPrec $ M.keys chainOpSpans
      assocGrouped :: [NEL.NonEmpty (NEL.NonEmpty (Qualified (OpName nameType)))]
      assocGrouped = fmap (NEL.groupWith1 opAssoc . NEL.sortWith opAssoc) precGrouped
      mixedAssoc :: [NEL.NonEmpty (Qualified (OpName nameType))]
      mixedAssoc = fmap join . filter (\precGroup -> NEL.length precGroup > 1) $ assocGrouped
      nonAssoc :: [NEL.NonEmpty (Qualified (OpName nameType))]
      nonAssoc = join $ fmap (NEL.filter (\assocGroup -> opAssoc (NEL.head assocGroup) == Infix && sum (fmap opUsages assocGroup) > 1)) assocGrouped
    in
      if null (nonAssoc ++ mixedAssoc)
        then internalError "matchOperators: cannot reorder operators"
        else
          map
            (\grp ->
              mkPositionedError chainOpSpans grp
                (MixedAssociativityError (fmap (\name -> (eraseOpName <$> name, opAssoc name)) grp)))
            mixedAssoc
          ++ map
            (\grp ->
              mkPositionedError chainOpSpans grp
                (NonAssociativeError (fmap (fmap eraseOpName) grp)))
            nonAssoc

  mkPositionedError
    :: M.Map (Qualified (OpName nameType)) (NEL.NonEmpty SourceSpan)
    -> NEL.NonEmpty (Qualified (OpName nameType))
    -> SimpleErrorMessage
    -> ErrorMessage
  mkPositionedError chainOpSpans grp =
    ErrorMessage
      [PositionedError (join . fmap (fromJust . flip M.lookup chainOpSpans) $ grp)]
