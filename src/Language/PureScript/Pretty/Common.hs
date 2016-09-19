{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Common pretty-printing utility functions
--
module Language.PureScript.Pretty.Common where

import Prelude.Compat

import Control.Monad.State (StateT, modify, get)

import Data.List (elemIndices, intersperse)

import Language.PureScript.AST (SourcePos(..), SourceSpan(..))
import Language.PureScript.Parser.Lexer (reservedPsNames, isUnquotedKey)

import Text.PrettyPrint.Boxes

-- |
-- Wrap a string in parentheses
--
parens :: String -> String
parens s = '(':s ++ ")"

parensPos :: (Emit gen) => gen -> gen
parensPos s = emit "(" `mappend` s `mappend` emit ")"

-- |
-- Generalize intercalate slightly for monoids
--
intercalate :: Monoid m => m -> [m] -> m
intercalate x xs = mconcat (intersperse x xs)

class (Monoid gen) => Emit gen where
  emit :: String -> gen
  addMapping :: SourceSpan -> gen

data SMap = SMap String SourcePos SourcePos

-- |
-- String with length and source-map entries
--
newtype StrPos = StrPos (SourcePos, String, [SMap])

-- |
-- Make a monoid where append consists of concatenating the string part, adding the lengths
-- appropriately and advancing source mappings on the right hand side to account for
-- the length of the left.
--
instance Monoid StrPos where
  mempty = StrPos (SourcePos 0 0, "", [])

  StrPos (a,b,c) `mappend` StrPos (a',b',c') = StrPos (a `addPos` a', b ++ b', c ++ (bumpPos a <$> c'))

  mconcat ms =
    let s' = concatMap (\(StrPos(_, s, _)) -> s) ms
        (p, maps) = foldl plus (SourcePos 0 0, []) ms
    in
        StrPos (p, s', concat $ reverse maps)
    where
      plus :: (SourcePos, [[SMap]]) -> StrPos -> (SourcePos, [[SMap]])
      plus (a, c) (StrPos (a', _, c')) = (a `addPos` a', (bumpPos a <$> c') : c)

instance Emit StrPos where
  -- |
  -- Augment a string with its length (rows/column)
  --
  emit str =
    let newlines = elemIndices '\n' str
        index = if null newlines then 0 else last newlines + 1
    in
    StrPos (SourcePos { sourcePosLine = length newlines, sourcePosColumn = length str - index }, str, [])

  -- |
  -- Add a new mapping entry for given source position with initially zero generated position
  --
  addMapping SourceSpan { spanName = file, spanStart = startPos } = StrPos (zeroPos, mempty, [mapping])
    where
      mapping = SMap file startPos zeroPos
      zeroPos = SourcePos 0 0

newtype PlainString = PlainString String deriving Monoid

runPlainString :: PlainString -> String
runPlainString (PlainString s) = s

instance Emit PlainString where
  emit = PlainString
  addMapping _ = mempty

addMapping' :: (Emit gen) => Maybe SourceSpan -> gen
addMapping' (Just ss) = addMapping ss
addMapping' Nothing = mempty

bumpPos :: SourcePos -> SMap -> SMap
bumpPos p (SMap f s g) = SMap f s $ p `addPos` g

addPos :: SourcePos -> SourcePos -> SourcePos
addPos (SourcePos n m) (SourcePos 0 m') = SourcePos n (m+m')
addPos (SourcePos n _) (SourcePos n' m') = SourcePos (n+n') m'


data PrinterState = PrinterState { indent :: Int }

emptyPrinterState :: PrinterState
emptyPrinterState = PrinterState { indent = 0 }

-- |
-- Number of characters per identation level
--
blockIndent :: Int
blockIndent = 4

-- |
-- Pretty print with a new indentation level
--
withIndent :: StateT PrinterState Maybe gen -> StateT PrinterState Maybe gen
withIndent action = do
  modify $ \st -> st { indent = indent st + blockIndent }
  result <- action
  modify $ \st -> st { indent = indent st - blockIndent }
  return result

-- |
-- Get the current indentation level
--
currentIndent :: (Emit gen) => StateT PrinterState Maybe gen
currentIndent = do
  current <- get
  return $ emit $ replicate (indent current) ' '

-- |
-- Print many lines
--
prettyPrintMany :: (Emit gen) => (a -> StateT PrinterState Maybe gen) -> [a] -> StateT PrinterState Maybe gen
prettyPrintMany f xs = do
  ss <- mapM f xs
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map (mappend indentString) ss

-- |
-- Prints an object key, escaping reserved names.
--
prettyPrintObjectKey :: String -> String
prettyPrintObjectKey s | s `elem` reservedPsNames = show s
                       | isUnquotedKey s = s
                       | otherwise = show s

-- | Place a box before another, vertically when the first box takes up multiple lines.
before :: Box -> Box -> Box
before b1 b2 | rows b1 > 1 = b1 // b2
             | otherwise = b1 <> b2

beforeWithSpace :: Box -> Box -> Box
beforeWithSpace b1 = before (b1 <> text " ")

-- | Place a Box on the bottom right of another
endWith :: Box -> Box -> Box
endWith l r = l <> vcat top [emptyBox (rows l - 1) (cols r), r]
