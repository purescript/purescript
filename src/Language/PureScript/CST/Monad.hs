module Language.PureScript.CST.Monad where

import Prelude

import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Ord (comparing)
import Data.Text (Text)
import Language.PureScript.CST.Errors (ParserError, ParserErrorInfo(..), ParserErrorType(..), ParserWarning, ParserWarningType)
import Language.PureScript.CST.Layout (LayoutStack)
import Language.PureScript.CST.Positions (widen)
import Language.PureScript.CST.Types (Comment, LineFeed, SourcePos(..), SourceRange(..), SourceToken(..), Token, TokenAnn(..))

type LexResult = Either (LexState, ParserError) SourceToken

data LexState = LexState
  { lexPos :: SourcePos
  , lexLeading :: [Comment LineFeed]
  , lexSource :: Text
  , lexStack :: LayoutStack
  } deriving (Show)

data ParserState = ParserState
  { parserBuff :: [LexResult]
  , parserErrors :: [ParserError]
  , parserWarnings :: [ParserWarning]
  } deriving (Show)

-- | A bare bones, CPS'ed `StateT s (Except e) a`.
newtype ParserM e s a =
  Parser (forall r. s -> (s -> e -> r) -> (s -> a -> r) -> r)

type Parser = ParserM ParserError ParserState

instance Functor (ParserM e s) where
  {-# INLINE fmap #-}
  fmap f (Parser k) =
    Parser $ \st kerr ksucc ->
      k st kerr (\st' a -> ksucc st' (f a))

instance Applicative (ParserM e s) where
  {-# INLINE pure #-}
  pure a = Parser $ \st _ k -> k st a
  {-# INLINE (<*>) #-}
  Parser k1 <*> Parser k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' f ->
        k2 st' kerr $ \st'' a ->
          ksucc st'' (f a)

instance Monad (ParserM e s) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  Parser k1 >>= k2 =
    Parser $ \st kerr ksucc ->
      k1 st kerr $ \st' a -> do
        let Parser k3 = k2 a
        k3 st' kerr ksucc

runParser :: ParserState -> Parser a -> (ParserState, Either (NE.NonEmpty ParserError) a)
runParser st (Parser k) = k st left right
  where
  left st'@ParserState {..} err =
    (st', Left $ NE.sortBy (comparing errRange) $ err NE.:| parserErrors)

  right st'@ParserState {..} res
    | null parserErrors = (st', Right res)
    | otherwise = (st', Left $ NE.fromList $ sortOn errRange parserErrors)

runTokenParser :: Parser a -> [LexResult] -> Either (NE.NonEmpty ParserError) ([ParserWarning], a)
runTokenParser p buff = fmap (warnings,) res
  where
  (ParserState _ _ warnings, res) =
    runParser initialState p

  initialState = ParserState
    { parserBuff = buff
    , parserErrors = []
    , parserWarnings = []
    }

{-# INLINE throw #-}
throw :: e -> ParserM e s a
throw e = Parser $ \st kerr _ -> kerr st e

parseError :: SourceToken -> Parser a
parseError tok = Parser $ \st kerr _ ->
  kerr st $ ParserErrorInfo
    { errRange = tokRange . tokAnn $ tok
    , errToks = [tok]
    , errStack = [] -- TODO parserStack st
    , errType = ErrToken
    }

mkParserError :: LayoutStack -> [SourceToken] -> a -> ParserErrorInfo a
mkParserError stack toks ty =
  ParserErrorInfo
    { errRange =  range
    , errToks = toks
    , errStack = stack
    , errType = ty
    }
  where
  range = case NE.nonEmpty toks of
    Nothing -> SourceRange (SourcePos 0 0) (SourcePos 0 0)
    Just neToks -> widen
     (tokRange . tokAnn $ NE.head neToks)
     (tokRange . tokAnn $ NE.last neToks)

addFailure :: [SourceToken] -> ParserErrorType -> Parser ()
addFailure toks ty = Parser $ \st _ ksucc ->
  ksucc (st { parserErrors = mkParserError [] toks ty : parserErrors st }) ()

parseFail' :: [SourceToken] -> ParserErrorType -> Parser a
parseFail' toks msg = Parser $ \st kerr _ -> kerr st (mkParserError [] toks msg)

parseFail :: SourceToken -> ParserErrorType -> Parser a
parseFail = parseFail' . pure

addWarning :: [SourceToken] -> ParserWarningType -> Parser ()
addWarning toks ty = Parser $ \st _ ksucc ->
  ksucc (st { parserWarnings = mkParserError [] toks ty : parserWarnings st }) ()

pushBack :: SourceToken -> Parser ()
pushBack tok = Parser $ \st _ ksucc ->
  ksucc (st { parserBuff = Right tok : parserBuff st }) ()

{-# INLINE tryPrefix #-}
tryPrefix :: Parser a -> Parser b -> Parser (Maybe a, b)
tryPrefix (Parser lhs) rhs = Parser $ \st kerr ksucc ->
  lhs st
    (\_ _ -> do
      let Parser k = (Nothing,) <$> rhs
      k st kerr ksucc)
    (\st' res -> do
      let Parser k = (Just res,) <$> rhs
      k st' kerr ksucc)

oneOf :: NE.NonEmpty (Parser a) -> Parser a
oneOf parsers = Parser $ \st kerr ksucc -> do
  let
    prevErrs = parserErrors st
    go (st', Right a) _ = (st', Right a)
    go _ (st', Right a) = (st', Right a)
    go (st1, Left errs1) (st2, Left errs2)
      | errRange (NE.last errs2) > errRange (NE.last errs1) = (st2, Left errs2)
      | otherwise = (st1, Left errs1)
  case foldr1 go $ runParser (st { parserErrors = [] }) <$> parsers of
    (st', Left errs) -> kerr (st' { parserErrors = prevErrs <> NE.tail errs}) $ NE.head errs
    (st', Right res) -> ksucc (st' { parserErrors = prevErrs }) res

manyDelimited :: Token -> Token -> Token -> Parser a -> Parser [a]
manyDelimited open close sep p = do
  _   <- token open
  res <- go1
  _   <- token close
  pure res
  where
  go1 =
    oneOf $ NE.fromList
      [ go2 . pure =<< p
      , pure []
      ]

  go2 acc =
    oneOf $ NE.fromList
      [ token sep *> (go2 . (: acc) =<< p)
      , pure (reverse acc)
      ]

token :: Token -> Parser SourceToken
token t = do
  t' <- munch
  if t == tokValue t'
    then pure t'
    else parseError t'

munch :: Parser SourceToken
munch = Parser $ \state@ParserState {..} kerr ksucc ->
  case parserBuff of
    Right tok : parserBuff' ->
      ksucc (state { parserBuff = parserBuff' }) tok
    Left (_,  err) : _ ->
      kerr state err
    [] ->
      error "Empty input"
