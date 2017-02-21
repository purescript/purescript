-- |
-- Parser for PSCI.
--
module Language.PureScript.Interactive.Parser
  ( parseCommand
  ) where

import           Prelude.Compat hiding (lex)

import           Data.Bifunctor (first)
import           Data.Char (isSpace)
import           Data.List (intercalate)
import qualified Data.Text as T
import           Text.Parsec hiding ((<|>))
import qualified Language.PureScript as P
import qualified Language.PureScript.Interactive.Directive as D
import           Language.PureScript.Interactive.Types
import           Language.PureScript.Parser.Common (mark, same)

-- |
-- Parses PSCI metacommands or expressions input from the user.
--
parseCommand :: String -> Either String Command
parseCommand cmdString =
  case cmdString of
    (':' : cmd) -> parseDirective cmd
    _ -> parseRest psciCommand cmdString

parseRest :: P.TokenParser a -> String -> Either String a
parseRest p s = first show $ do
  ts <- P.lex "" (T.pack s)
  P.runTokenParser "" (p <* eof) ts

psciCommand :: P.TokenParser Command
psciCommand = choice (map try parsers)
  where
  parsers =
    [ psciImport
    , psciDeclaration
    , psciExpression
    , psciDeprecatedLet
    ]

trim :: String -> String
trim = trimEnd . trimStart

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse

parseDirective :: String -> Either String Command
parseDirective cmd =
  case D.directivesFor' dstr of
    [(d, _)] -> commandFor d
    []       -> Left "Unrecognized directive. Type :? for help."
    ds       -> Left ("Ambiguous directive. Possible matches: " ++
                  intercalate ", " (map snd ds) ++ ". Type :? for help.")
  where
  (dstr, arg) = break isSpace cmd

  commandFor d = case d of
    Help    -> return ShowHelp
    Quit    -> return QuitPSCi
    Reset   -> return ResetState
    Paste   -> return PasteLines
    Browse  -> BrowseModule <$> parseRest P.moduleName arg
    Show    -> ShowInfo <$> parseReplQuery' (trim arg)
    Type    -> TypeOf <$> parseRest P.parseValue arg
    Kind    -> KindOf <$> parseRest P.parseType arg

-- |
-- Parses expressions entered at the PSCI repl.
--
psciExpression :: P.TokenParser Command
psciExpression = Expression <$> P.parseValue

-- | Imports must be handled separately from other declarations, so that
-- :show import works, for example.
psciImport :: P.TokenParser Command
psciImport = do
  (mn, declType, asQ) <- P.parseImportDeclaration'
  return $ Import (mn, declType, asQ)

-- | Any declaration that we don't need a 'special case' parser for
-- (like import declarations).
psciDeclaration :: P.TokenParser Command
psciDeclaration = fmap Decls $ mark $ many1 $ same *> do
  decl <- discardPositionInfo <$> P.parseDeclaration
  if acceptable decl
    then return decl
    else fail "this kind of declaration is not supported in psci"

discardPositionInfo :: P.Declaration -> P.Declaration
discardPositionInfo (P.PositionedDeclaration _ _ d) = d
discardPositionInfo d = d

acceptable :: P.Declaration -> Bool
acceptable P.DataDeclaration{} = True
acceptable P.TypeSynonymDeclaration{} = True
acceptable P.ExternDeclaration{} = True
acceptable P.ExternDataDeclaration{} = True
acceptable P.TypeClassDeclaration{} = True
acceptable P.TypeInstanceDeclaration{} = True
acceptable P.ExternKindDeclaration{} = True
acceptable P.TypeDeclaration{} = True
acceptable P.ValueDeclaration{} = True
acceptable _ = False

parseReplQuery' :: String -> Either String ReplQuery
parseReplQuery' str =
  case parseReplQuery str of
    Nothing -> Left ("Don't know how to show " ++ str ++ ". Try one of: " ++
                      intercalate ", " replQueryStrings ++ ".")
    Just query -> Right query

-- | To show error message when 'let' is used for declaration in PSCI,
-- which is deprecated.
psciDeprecatedLet :: P.TokenParser Command
psciDeprecatedLet = do
  P.reserved "let"
  P.indented
  _ <- mark (many1 (same *> P.parseLocalDeclaration))
  notFollowedBy $ P.reserved "in"
  fail "Declarations in PSCi no longer require \"let\", as of version 0.11.0"
