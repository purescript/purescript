-- | Useful common functions for building parsers
module Language.PureScript.Parser.Common where

import           Prelude.Compat

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Language.PureScript.AST.SourcePos
import           Language.PureScript.Comments
import           Language.PureScript.Names
import           Language.PureScript.Parser.Lexer
import           Language.PureScript.Parser.State
import           Language.PureScript.PSString (PSString, mkString)
import qualified Text.Parsec as P

-- | Parse a general proper name.
properName :: TokenParser (ProperName a)
properName = ProperName <$> uname

-- | Parse a proper name for a type.
typeName :: TokenParser (ProperName 'TypeName)
typeName = ProperName <$> tyname

-- | Parse a proper name for a kind.
kindName :: TokenParser (ProperName 'KindName)
kindName = ProperName <$> kiname

-- | Parse a proper name for a data constructor.
dataConstructorName :: TokenParser (ProperName 'ConstructorName)
dataConstructorName = ProperName <$> dconsname

-- | Parse a module name
moduleName :: TokenParser ModuleName
moduleName = part []
  where
  part path = (do name <- ProperName <$> P.try qualifier
                  part (path `snoc` name))
              <|> (ModuleName . snoc path . ProperName <$> mname)
  snoc path name = path ++ [name]

-- | Parse a qualified name, i.e. M.name or just name
parseQualified :: TokenParser a -> TokenParser (Qualified a)
parseQualified parser = part []
  where
  part path = (do name <- ProperName <$> P.try qualifier
                  part (updatePath path name))
              <|> (Qualified (qual path) <$> P.try parser)
  updatePath path name = path ++ [name]
  qual path = if null path then Nothing else Just $ ModuleName path

-- | Parse an identifier.
parseIdent :: TokenParser Ident
parseIdent = Ident <$> identifier

-- | Parse a label, which may look like an identifier or a string
parseLabel :: TokenParser PSString
parseLabel = (mkString <$> lname) <|> stringLiteral

-- | Parse an operator.
parseOperator :: TokenParser (OpName a)
parseOperator = OpName <$> symbol

-- | Run the first parser, then match the second if possible, applying the specified function on a successful match
augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = flip (maybe id $ flip f) <$> p <*> P.optionMaybe q

-- | Run the first parser, then match the second zero or more times, applying the specified function for each match
fold :: P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first' more combine = do
  a <- first'
  bs <- P.many more
  return $ foldl combine a bs

-- | Build a parser from a smaller parser and a list of parsers for postfix operators
buildPostfixParser :: P.Stream s m t => [a -> P.ParsecT s u m a] -> P.ParsecT s u m a -> P.ParsecT s u m a
buildPostfixParser fs first' = do
  a <- first'
  go a
  where
  go a = do
    maybeA <- P.optionMaybe $ P.choice (map ($ a) fs)
    case maybeA of
      Nothing -> return a
      Just a' -> go a'

-- | Mark the current indentation level
mark :: P.Parsec s ParseState a -> P.Parsec s ParseState a
mark p = do
  current <- indentationLevel <$> P.getState
  pos <- P.sourceColumn <$> P.getPosition
  P.modifyState $ \st -> st { indentationLevel = pos }
  a <- p
  P.modifyState $ \st -> st { indentationLevel = current }
  return a

-- | Check that the current identation level matches a predicate
checkIndentation
  :: (P.Column -> Text)
  -> (P.Column -> P.Column -> Bool)
  -> P.Parsec s ParseState ()
checkIndentation mkMsg rel = do
  col <- P.sourceColumn <$> P.getPosition
  current <- indentationLevel <$> P.getState
  guard (col `rel` current) P.<?> T.unpack (mkMsg current)

-- | Check that the current indentation level is past the current mark
indented :: P.Parsec s ParseState ()
indented = checkIndentation (("indentation past column " <>) . (T.pack . show)) (>)

-- | Check that the current indentation level is at the same indentation as the current mark
same :: P.Parsec s ParseState ()
same = checkIndentation (("indentation at column " <>) . (T.pack . show)) (==)

-- | Read the comments from the the next token, without consuming it
readComments :: P.Parsec [PositionedToken] u [Comment]
readComments = P.lookAhead $ ptComments <$> P.anyToken

-- | Run a parser
runTokenParser :: FilePath -> TokenParser a -> [PositionedToken] -> Either P.ParseError a
runTokenParser filePath p = P.runParser p (ParseState 0) filePath

-- | Convert from Parsec sourcepos
toSourcePos :: P.SourcePos -> SourcePos
toSourcePos pos = SourcePos (P.sourceLine pos) (P.sourceColumn pos)

-- | Read source position information and comments
withSourceSpan
  :: (SourceSpan -> [Comment] -> a -> b)
  -> P.Parsec [PositionedToken] u a
  -> P.Parsec [PositionedToken] u b
withSourceSpan f p = do
  comments <- readComments
  start <- P.getPosition
  x <- p
  end <- P.getPosition
  input <- P.getInput
  let end' = case input of
        pt:_ -> ptPrevEndPos pt
        _ -> Nothing
  let sp = SourceSpan (P.sourceName start) (toSourcePos start) (toSourcePos $ fromMaybe end end')
  return $ f sp comments x

withSourceAnnF
  :: P.Parsec [PositionedToken] u (SourceAnn -> a)
  -> P.Parsec [PositionedToken] u a
withSourceAnnF = withSourceSpan (\ss com f -> f (ss, com))

withSourceSpan'
  :: (SourceSpan -> a -> b)
  -> P.Parsec [PositionedToken] u a
  -> P.Parsec [PositionedToken] u b
withSourceSpan' f = withSourceSpan (\ss _ -> f ss)
