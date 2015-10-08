-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.Pretty.Common
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- Common pretty-printing utility functions
--
-----------------------------------------------------------------------------

module Language.PureScript.Pretty.Common where

import Control.Monad.State
import Data.List (intercalate)

import Language.PureScript.Parser.Lexer (reservedPsNames, opChars)

import Text.PrettyPrint.Boxes

-- |
-- Wrap a string in parentheses
--
parens :: String -> String
parens s = ('(':s) ++ ")"

newtype PrinterState = PrinterState { indent :: Int } deriving (Show, Read, Eq, Ord)

-- |
-- Number of characters per identation level
--
blockIndent :: Int
blockIndent = 4

-- |
-- Pretty print with a new indentation level
--
withIndent :: StateT PrinterState Maybe String -> StateT PrinterState Maybe String
withIndent action = do
  modify $ \st -> st { indent = indent st + blockIndent }
  result <- action
  modify $ \st -> st { indent = indent st - blockIndent }
  return result

-- |
-- Get the current indentation level
--
currentIndent :: StateT PrinterState Maybe String
currentIndent = do
  current <- get
  return $ replicate (indent current) ' '

-- |
-- Print many lines
--
prettyPrintMany :: (a -> StateT PrinterState Maybe String) -> [a] -> StateT PrinterState Maybe String
prettyPrintMany f xs = do
  ss <- mapM f xs
  indentString <- currentIndent
  return $ intercalate "\n" $ map (indentString ++) ss

-- |
-- Prints an object key, escaping reserved names.
--
prettyPrintObjectKey :: String -> String
prettyPrintObjectKey s | s `elem` reservedPsNames = show s
                       | any (`elem` opChars) s = show s
                       | otherwise = s

-- | Place a box before another, vertically when the first box takes up multiple lines.
before :: Box -> Box -> Box
before b1 b2 | rows b1 > 1 = b1 // b2
             | otherwise = b1 <> b2

beforeWithSpace :: Box -> Box -> Box
beforeWithSpace b1 = before (b1 <> text " ")
