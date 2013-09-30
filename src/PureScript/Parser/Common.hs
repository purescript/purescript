-----------------------------------------------------------------------------
--
-- Module      :  PureScript.Parser.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module PureScript.Parser.Common where

import Control.Applicative
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Language as PL

langDef = PL.haskellStyle
  { PT.identStart    = P.lower <|> P.char '_'
  , PT.reservedNames = [ "case"
                       , "data"
                       , "var"
                       , "while"
                       , "for"
                       , "if"
                       , "then"
                       , "else"
                       , "return"
                       , "true"
                       , "false" ]
  }

tokenParser      = PT.makeTokenParser   langDef
lexeme           = PT.lexeme            tokenParser
identifier       = PT.identifier        tokenParser
reserved         = PT.reserved          tokenParser
operator         = PT.operator          tokenParser
stringLiteral    = PT.stringLiteral     tokenParser
natural          = PT.natural           tokenParser
naturalOrFloat   = PT.naturalOrFloat    tokenParser
whiteSpace       = PT.whiteSpace        tokenParser
parens           = PT.parens            tokenParser
braces           = PT.braces            tokenParser
angles           = PT.angles            tokenParser
brackets         = PT.brackets          tokenParser
squares          = PT.squares           tokenParser
semi             = PT.semi              tokenParser
comma            = PT.comma             tokenParser
colon            = PT.colon             tokenParser
dot              = PT.dot               tokenParser
semiSep          = PT.semiSep           tokenParser
semiSep1         = PT.semiSep1          tokenParser
commaSep         = PT.commaSep          tokenParser
commaSep1        = PT.commaSep1         tokenParser

properName :: P.Parsec String u String
properName = lexeme $ P.try properName'
  where
  properName' = (:) <$> P.upper <*> many (PT.identLetter langDef) P.<?> "name"

augment :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
augment p q f = (flip $ maybe id $ flip f) <$> p <*> P.optionMaybe q

fold :: P.Stream s m t => P.ParsecT s u m a -> P.ParsecT s u m b -> (a -> b -> a) -> P.ParsecT s u m a
fold first more combine = do
  a <- first
  bs <- P.many more
  return $ foldl combine a bs
