-- |
-- The first step in the parsing process - turns source code into a list of lexemes
--
module Language.PureScript.Parser.Lexer
  ( PositionedToken(..)
  , Token()
  , TokenParser()
  , lex
  , anyToken
  , token
  , match
  , lparen
  , rparen
  , parens
  , lbrace
  , rbrace
  , braces
  , lsquare
  , rsquare
  , squares
  , indent
  , indentAt
  , larrow
  , rarrow
  , lfatArrow
  , rfatArrow
  , colon
  , doubleColon
  , equals
  , pipe
  , tick
  , dot
  , comma
  , semi
  , at
  , underscore
  , holeLit
  , semiSep
  , semiSep1
  , commaSep
  , commaSep1
  , lname
  , lname'
  , qualifier
  , tyname
  , uname
  , uname'
  , mname
  , reserved
  , symbol
  , symbol'
  , identifier
  , charLiteral
  , stringLiteral
  , number
  , natural
  , reservedPsNames
  , reservedTypeNames
  , isSymbolChar
  , isUnquotedKey
  )
  where

import Prelude hiding (lex)

import Control.Applicative
import Control.Monad (void, guard)

import Data.Char (isSpace, isAscii, isSymbol, isAlphaNum)
import Data.Functor.Identity

import Language.PureScript.Comments
import Language.PureScript.Parser.State

import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT

data Token
  = LParen
  | RParen
  | LBrace
  | RBrace
  | LSquare
  | RSquare
  | Indent Int
  | LArrow
  | RArrow
  | LFatArrow
  | RFatArrow
  | Colon
  | DoubleColon
  | Equals
  | Pipe
  | Tick
  | Dot
  | Comma
  | Semi
  | At
  | Underscore
  | LName String
  | UName String
  | Qualifier String
  | Symbol String
  | CharLiteral Char
  | StringLiteral String
  | Number (Either Integer Double)
  | HoleLit String
  deriving (Show, Eq, Ord)

prettyPrintToken :: Token -> String
prettyPrintToken LParen            = "("
prettyPrintToken RParen            = ")"
prettyPrintToken LBrace            = "{"
prettyPrintToken RBrace            = "}"
prettyPrintToken LSquare           = "["
prettyPrintToken RSquare           = "]"
prettyPrintToken LArrow            = "<-"
prettyPrintToken RArrow            = "->"
prettyPrintToken LFatArrow         = "<="
prettyPrintToken RFatArrow         = "=>"
prettyPrintToken Colon             = ":"
prettyPrintToken DoubleColon       = "::"
prettyPrintToken Equals            = "="
prettyPrintToken Pipe              = "|"
prettyPrintToken Tick              = "`"
prettyPrintToken Dot               = "."
prettyPrintToken Comma             = ","
prettyPrintToken Semi              = ";"
prettyPrintToken At                = "@"
prettyPrintToken Underscore        = "_"
prettyPrintToken (Indent n)        = "indentation at level " ++ show n
prettyPrintToken (LName s)         = show s
prettyPrintToken (UName s)         = show s
prettyPrintToken (Qualifier _)     = "qualifier"
prettyPrintToken (Symbol s)        = s
prettyPrintToken (CharLiteral c)   = show c
prettyPrintToken (StringLiteral s) = show s
prettyPrintToken (Number n)        = either show show n
prettyPrintToken (HoleLit name)    = "?" ++ name

data PositionedToken = PositionedToken
  { -- | Start position of this token
    ptSourcePos :: P.SourcePos
    -- | End position of this token (not including whitespace)
  , ptEndPos :: P.SourcePos
    -- | End position of the previous token
  , ptPrevEndPos :: Maybe P.SourcePos
  , ptToken     :: Token
  , ptComments  :: [Comment]
  } deriving (Eq)

-- Parsec requires this instance for various token-level combinators
instance Show PositionedToken where
  show = prettyPrintToken . ptToken

lex :: FilePath -> String -> Either P.ParseError [PositionedToken]
lex f s = updatePositions <$> P.parse parseTokens f s

updatePositions :: [PositionedToken] -> [PositionedToken]
updatePositions [] = []
updatePositions (x:xs) = x : zipWith update (x:xs) xs
  where
  update PositionedToken { ptEndPos = pos } pt = pt { ptPrevEndPos = Just pos }

parseTokens :: P.Parsec String u [PositionedToken]
parseTokens = whitespace *> P.many parsePositionedToken <* P.skipMany parseComment <* P.eof

whitespace :: P.Parsec String u ()
whitespace = P.skipMany (P.satisfy isSpace)

parseComment :: P.Parsec String u Comment
parseComment = (BlockComment <$> blockComment <|> LineComment <$> lineComment) <* whitespace
  where
  blockComment :: P.Parsec String u String
  blockComment = P.try $ P.string "{-" *> P.manyTill P.anyChar (P.try (P.string "-}"))

  lineComment :: P.Parsec String u String
  lineComment = P.try $ P.string "--" *> P.manyTill P.anyChar (P.try (void (P.char '\n') <|> P.eof))

parsePositionedToken :: P.Parsec String u PositionedToken
parsePositionedToken = P.try $ do
  comments <- P.many parseComment
  pos <- P.getPosition
  tok <- parseToken
  pos' <- P.getPosition
  whitespace
  return $ PositionedToken pos pos' Nothing tok comments

parseToken :: P.Parsec String u Token
parseToken = P.choice
  [ P.try $ P.string "<-" *> P.notFollowedBy symbolChar *> pure LArrow
  , P.try $ P.string "←"  *> P.notFollowedBy symbolChar *> pure LArrow
  , P.try $ P.string "<=" *> P.notFollowedBy symbolChar *> pure LFatArrow
  , P.try $ P.string "⇐"  *> P.notFollowedBy symbolChar *> pure LFatArrow
  , P.try $ P.string "->" *> P.notFollowedBy symbolChar *> pure RArrow
  , P.try $ P.string "→"  *> P.notFollowedBy symbolChar *> pure RArrow
  , P.try $ P.string "=>" *> P.notFollowedBy symbolChar *> pure RFatArrow
  , P.try $ P.string "⇒"  *> P.notFollowedBy symbolChar *> pure RFatArrow
  , P.try $ P.string "::" *> P.notFollowedBy symbolChar *> pure DoubleColon
  , P.try $ P.string "∷"  *> P.notFollowedBy symbolChar *> pure DoubleColon
  , P.try $ P.char '('    *> pure LParen
  , P.try $ P.char ')'    *> pure RParen
  , P.try $ P.char '{'    *> pure LBrace
  , P.try $ P.char '}'    *> pure RBrace
  , P.try $ P.char '['    *> pure LSquare
  , P.try $ P.char ']'    *> pure RSquare
  , P.try $ P.char '`'    *> pure Tick
  , P.try $ P.char ','    *> pure Comma
  , P.try $ P.char '='    *> P.notFollowedBy symbolChar *> pure Equals
  , P.try $ P.char ':'    *> P.notFollowedBy symbolChar *> pure Colon
  , P.try $ P.char '|'    *> P.notFollowedBy symbolChar *> pure Pipe
  , P.try $ P.char '.'    *> P.notFollowedBy symbolChar *> pure Dot
  , P.try $ P.char ';'    *> P.notFollowedBy symbolChar *> pure Semi
  , P.try $ P.char '@'    *> P.notFollowedBy symbolChar *> pure At
  , P.try $ P.char '_'    *> P.notFollowedBy identLetter *> pure Underscore
  , HoleLit <$> P.try (P.char '?' *> P.many1 identLetter)
  , LName         <$> parseLName
  , parseUName >>= \uName ->
      (guard (validModuleName uName) >> Qualifier uName <$ P.char '.')
        <|> pure (UName uName)
  , Symbol        <$> parseSymbol
  , CharLiteral   <$> parseCharLiteral
  , StringLiteral <$> parseStringLiteral
  , Number        <$> parseNumber
  ]

  where
  parseLName :: P.Parsec String u String
  parseLName = (:) <$> identStart <*> P.many identLetter

  parseUName :: P.Parsec String u String
  parseUName = (:) <$> P.upper <*> P.many identLetter

  parseSymbol :: P.Parsec String u String
  parseSymbol = P.many1 symbolChar

  identStart :: P.Parsec String u Char
  identStart = P.lower <|> P.oneOf "_"

  identLetter :: P.Parsec String u Char
  identLetter = P.alphaNum <|> P.oneOf "_'"

  symbolChar :: P.Parsec String u Char
  symbolChar = P.satisfy isSymbolChar

  parseCharLiteral :: P.Parsec String u Char
  parseCharLiteral = PT.charLiteral tokenParser

  parseStringLiteral :: P.Parsec String u String
  parseStringLiteral = blockString <|> PT.stringLiteral tokenParser
    where
    delimiter   = P.try (P.string "\"\"\"")
    blockString = delimiter >> P.manyTill P.anyChar delimiter

  parseNumber :: P.Parsec String u (Either Integer Double)
  parseNumber = (consumeLeadingZero >> P.parserZero) <|>
                  (Right <$> P.try (PT.float tokenParser) <|>
                  Left <$> P.try (PT.natural tokenParser))
                P.<?> "number"
    where
    -- lookAhead doesn't consume any input if its parser succeeds
    -- if notFollowedBy fails though, the consumed '0' will break the choice chain
    consumeLeadingZero = P.lookAhead (P.char '0' >>
      (P.notFollowedBy P.digit P.<?> "no leading zero in number literal"))

-- |
-- We use Text.Parsec.Token to implement the string and number lexemes
--
langDef :: PT.GenLanguageDef String u Identity
langDef = PT.LanguageDef
  { PT.reservedNames   = []
  , PT.reservedOpNames = []
  , PT.commentStart    = ""
  , PT.commentEnd      = ""
  , PT.commentLine     = ""
  , PT.nestedComments  = True
  , PT.identStart      = fail "Identifiers not supported"
  , PT.identLetter     = fail "Identifiers not supported"
  , PT.opStart         = fail "Operators not supported"
  , PT.opLetter        = fail "Operators not supported"
  , PT.caseSensitive   = True
  }

-- |
-- A token parser based on the language definition
--
tokenParser :: PT.GenTokenParser String u Identity
tokenParser = PT.makeTokenParser langDef

type TokenParser a = P.Parsec [PositionedToken] ParseState a

anyToken :: TokenParser PositionedToken
anyToken = P.token (prettyPrintToken . ptToken) ptSourcePos Just

token :: (Token -> Maybe a) -> TokenParser a
token f = P.token (prettyPrintToken . ptToken) ptSourcePos (f . ptToken)

match :: Token -> TokenParser ()
match tok = token (\tok' -> if tok == tok' then Just () else Nothing) P.<?> prettyPrintToken tok

lparen :: TokenParser ()
lparen = match LParen

rparen :: TokenParser ()
rparen = match RParen

parens :: TokenParser a -> TokenParser a
parens = P.between lparen rparen

lbrace :: TokenParser ()
lbrace = match LBrace

rbrace :: TokenParser ()
rbrace = match RBrace

braces :: TokenParser a -> TokenParser a
braces = P.between lbrace rbrace

lsquare :: TokenParser ()
lsquare = match LSquare

rsquare :: TokenParser ()
rsquare = match RSquare

squares :: TokenParser a -> TokenParser a
squares = P.between lsquare rsquare

indent :: TokenParser Int
indent = token go P.<?> "indentation"
  where
  go (Indent n) = Just n
  go _ = Nothing

indentAt :: P.Column -> TokenParser ()
indentAt n = token go P.<?> "indentation at level " ++ show n
  where
  go (Indent n') | n == n' = Just ()
  go _ = Nothing

larrow :: TokenParser ()
larrow = match LArrow

rarrow :: TokenParser ()
rarrow = match RArrow

lfatArrow :: TokenParser ()
lfatArrow = match LFatArrow

rfatArrow :: TokenParser ()
rfatArrow = match RFatArrow

colon :: TokenParser ()
colon = match Colon

doubleColon :: TokenParser ()
doubleColon = match DoubleColon

equals :: TokenParser ()
equals = match Equals

pipe :: TokenParser ()
pipe = match Pipe

tick :: TokenParser ()
tick = match Tick

dot :: TokenParser ()
dot = match Dot

comma :: TokenParser ()
comma = match Comma

semi :: TokenParser ()
semi = match Semi

at :: TokenParser ()
at = match At

underscore :: TokenParser ()
underscore = match Underscore

holeLit :: TokenParser String
holeLit = token go P.<?> "hole literal"
  where
  go (HoleLit n) = Just n
  go _ = Nothing

-- |
-- Parse zero or more values separated by semicolons
--
semiSep :: TokenParser a -> TokenParser [a]
semiSep = flip P.sepBy semi

-- |
-- Parse one or more values separated by semicolons
--
semiSep1 :: TokenParser a -> TokenParser [a]
semiSep1 = flip P.sepBy1 semi

-- |
-- Parse zero or more values separated by commas
--
commaSep :: TokenParser a -> TokenParser [a]
commaSep = flip P.sepBy comma

-- |
-- Parse one or more values separated by commas
--
commaSep1 :: TokenParser a -> TokenParser [a]
commaSep1 = flip P.sepBy1 comma

lname :: TokenParser String
lname = token go P.<?> "identifier"
  where
  go (LName s) = Just s
  go _ = Nothing

lname' :: String -> TokenParser ()
lname' s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go _ = Nothing

qualifier :: TokenParser String
qualifier = token go P.<?> "qualifier"
  where
  go (Qualifier s) = Just s
  go _ = Nothing

reserved :: String -> TokenParser ()
reserved s = token go P.<?> show s
  where
  go (LName s') | s == s' = Just ()
  go (Symbol s') | s == s' = Just ()
  go _ = Nothing

uname :: TokenParser String
uname = token go P.<?> "proper name"
  where
  go (UName s) | validUName s = Just s
  go _ = Nothing

uname' :: String -> TokenParser ()
uname' s = token go P.<?> "proper name"
  where
  go (UName s') | s == s' = Just ()
  go _ = Nothing

tyname :: TokenParser String
tyname = token go P.<?> "type name"
  where
  go (UName s) = Just s
  go _ = Nothing

mname :: TokenParser String
mname = token go P.<?> "module name"
  where
  go (UName s) | validModuleName s = Just s
  go _ = Nothing

symbol :: TokenParser String
symbol = token go P.<?> "symbol"
  where
  go (Symbol s) = Just s
  go Colon      = Just ":"
  go LFatArrow  = Just "<="
  go At         = Just "@"
  go _ = Nothing

symbol' :: String -> TokenParser ()
symbol' s = token go P.<?> show s
  where
  go (Symbol s') | s == s'   = Just ()
  go Colon       | s == ":"  = Just ()
  go LFatArrow   | s == "<=" = Just ()
  go _ = Nothing

charLiteral :: TokenParser Char
charLiteral = token go P.<?> "char literal"
  where
  go (CharLiteral c) = Just c
  go _ = Nothing

stringLiteral :: TokenParser String
stringLiteral = token go P.<?> "string literal"
  where
  go (StringLiteral s) = Just s
  go _ = Nothing

number :: TokenParser (Either Integer Double)
number = token go P.<?> "number"
  where
  go (Number n) = Just n
  go _ = Nothing

natural :: TokenParser Integer
natural = token go P.<?> "natural"
  where
  go (Number (Left n)) = Just n
  go _ = Nothing

identifier :: TokenParser String
identifier = token go P.<?> "identifier"
  where
  go (LName s) | s `notElem` reservedPsNames = Just s
  go _ = Nothing

validModuleName :: String -> Bool
validModuleName s = '_' `notElem` s

validUName :: String -> Bool
validUName s = '\'' `notElem` s

-- |
-- A list of purescript reserved identifiers
--
reservedPsNames :: [String]
reservedPsNames = [ "data"
                  , "newtype"
                  , "type"
                  , "foreign"
                  , "import"
                  , "infixl"
                  , "infixr"
                  , "infix"
                  , "class"
                  , "instance"
                  , "derive"
                  , "module"
                  , "case"
                  , "of"
                  , "if"
                  , "then"
                  , "else"
                  , "do"
                  , "let"
                  , "true"
                  , "false"
                  , "in"
                  , "where"
                  ]

reservedTypeNames :: [String]
reservedTypeNames = [ "forall", "where" ]

-- |
-- The characters allowed for use in operators
--
isSymbolChar :: Char -> Bool
isSymbolChar c = (c `elem` ":!#$%&*+./<=>?@\\^|-~") || (not (isAscii c) && isSymbol c)


-- |
-- The characters allowed in the head of an unquoted record key
--
isUnquotedKeyHeadChar :: Char -> Bool
isUnquotedKeyHeadChar c = (c == '_') || isAlphaNum c

-- |
-- The characters allowed in the tail of an unquoted record key
--
isUnquotedKeyTailChar :: Char -> Bool
isUnquotedKeyTailChar c = (c `elem` "_'") || isAlphaNum c

-- |
-- Strings allowed to be left unquoted in a record key
--
isUnquotedKey :: String -> Bool
isUnquotedKey []        = False
isUnquotedKey (hd : tl) = isUnquotedKeyHeadChar hd &&
                          all isUnquotedKeyTailChar tl
