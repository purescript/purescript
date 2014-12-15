-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Parser.Indentation
-- Copyright   :  (c) Phil Freeman 2013-14
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Parser combinators for working with indentation-sensitive code
--
-----------------------------------------------------------------------------

module Language.PureScript.Parser.Indentation (block, genBlock) where

import Text.Parsec

import Data.Maybe (fromMaybe)

import Control.Monad (guard)

import Language.PureScript.Parser.Lexer
import Language.PureScript.Parser.State
      
popUntil :: Int -> TokenParser ()
popUntil ref = do
  n <- indent
  modifyState $ popIndentationWhile (> n)
  guard (n == ref)
      
genBlock :: Bool -> TokenParser a -> TokenParser [a]
genBlock lenient p = do
  choice [ do n <- try $ do
                n <- indent
                st <- getState
                let cur = fromMaybe 1 $ peekIndentation st
                guard (n ?> cur) <?> "indentation past level " ++ show cur
                modifyState $ pushIndentation n
                return n
              first <- p
              rest <- many . try $ do 
                popUntil n <?> ("indentation at level " ++ show n)
                p
              return $ first : rest
         , return []
         ]
  where 
  x ?> y | lenient = x >= y
         | otherwise = x > y
     
block :: TokenParser a -> TokenParser [a]
block = genBlock False
