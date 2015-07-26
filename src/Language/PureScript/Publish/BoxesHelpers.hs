module Language.PureScript.Publish.BoxesHelpers
  ( Boxes.Box
  , Boxes.nullBox
  , module Language.PureScript.Publish.BoxesHelpers
  ) where

import System.IO (hPutStr, stderr)
import qualified Text.PrettyPrint.Boxes as Boxes

width :: Int
width = 79

indentWidth :: Int
indentWidth = 2

para :: String -> Boxes.Box
para = Boxes.para Boxes.left width

indented :: Boxes.Box -> Boxes.Box
indented b = Boxes.hcat Boxes.left [Boxes.emptyBox 1 indentWidth, b]

successivelyIndented :: [String] -> Boxes.Box
successivelyIndented [] =
  Boxes.nullBox
successivelyIndented (x:xs) =
  Boxes.vcat Boxes.left [para x, indented (successivelyIndented xs)]

vcat :: [Boxes.Box] -> Boxes.Box
vcat = Boxes.vcat Boxes.left

spacer :: Boxes.Box
spacer = Boxes.emptyBox 1 1

bulletedList :: (a -> String) -> [a] -> [Boxes.Box]
bulletedList f = map (indented . para . ("* " ++) . f)

printToStderr :: Boxes.Box -> IO ()
printToStderr = hPutStr stderr . Boxes.render
