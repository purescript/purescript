-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main where

import qualified Language.PureScript as P
import System.Console.CmdTheLine
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import System.Exit (exitSuccess, exitFailure)
import qualified System.IO.UTF8 as U
import qualified Paths_purescript as Paths
import Data.Version (showVersion)
import Data.List
import Data.Function (on)

docgen :: FilePath -> IO ()
docgen input = do
  text <- U.readFile input
  case P.runIndentParser input P.parseModules text of
    Left err -> do
      U.print err
      exitFailure
    Right ms -> do
      U.putStrLn . runDocs $ renderModules ms
      exitSuccess

type Docs = Writer [String] ()

runDocs :: Docs -> String
runDocs = unlines . execWriter

spacer :: Docs
spacer = tell [""]

headerLevel :: Int -> String -> Docs
headerLevel level hdr = tell [replicate level '#' ++ ' ' : hdr]

atIndent :: Int -> String -> Docs
atIndent indent text =
  let ls = lines text in
  forM_ ls $ \l -> tell [replicate indent ' ' ++ l]

renderModules :: [P.Module] -> Docs
renderModules ms = do
  headerLevel 1 "Module Documentation"
  mapM_ renderModule ms

renderModule :: P.Module -> Docs
renderModule (P.Module moduleName ds) = do
  headerLevel 2 $ "Module " ++ P.runModuleName moduleName
  spacer
  headerLevel 3 "Types"
  spacer
  renderTopLevel (filter isTypeDeclaration ds)
  spacer
  headerLevel 3 "Type Classes"
  spacer
  renderTopLevel (filter isTypeClassDeclaration ds)
  spacer
  headerLevel 3 "Type Class Instances"
  spacer
  renderTopLevel (filter isTypeInstanceDeclaration ds)
  spacer
  headerLevel 3 "Values"
  spacer
  renderTopLevel (filter isValueDeclaration ds)
  spacer

renderTopLevel :: [P.Declaration] -> Docs
renderTopLevel decls = forM_ (sortBy (compare `on` getName) decls) $ \decl -> do
  renderDeclaration 4 decl
  spacer

renderDeclaration :: Int -> P.Declaration -> Docs
renderDeclaration n (P.TypeDeclaration ident ty) =
  atIndent n $ show ident ++ " :: " ++ P.prettyPrintType ty
renderDeclaration n (P.ExternDeclaration _ ident _ ty) =
  atIndent n $ show ident ++ " :: " ++ P.prettyPrintType ty
renderDeclaration n (P.DataDeclaration name args ctors) = do
  let typeName = P.runProperName name ++ " " ++ unwords args
  atIndent n $ "data " ++ typeName ++ " where"
  forM_ ctors $ \(ctor, tys) ->
    atIndent (n + 2) $ P.runProperName ctor ++ " :: " ++ concatMap (\ty -> P.prettyPrintType ty ++ " -> ") tys ++ typeName
renderDeclaration n (P.ExternDataDeclaration name kind) =
  atIndent n $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind
renderDeclaration n (P.TypeSynonymDeclaration name args ty) = do
  let typeName = P.runProperName name ++ " " ++ unwords args
  atIndent n $ "type " ++ typeName ++ " = " ++ P.prettyPrintType ty
renderDeclaration n (P.TypeClassDeclaration name args ds) = do
  atIndent n $ "class " ++ P.runProperName name ++ " " ++ unwords args ++ " where"
  mapM_ (renderDeclaration (n + 2)) ds
renderDeclaration n (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate "," (map (\(pn, tys') -> show pn ++ " (" ++ unwords (map (("(" ++) . (++ ")") . P.prettyPrintType) tys') ++ ")") cs) ++ ") => "
  atIndent n $ constraintsText ++ "instance " ++ show name ++ " :: " ++ show className ++ " " ++ unwords (map (("(" ++) . (++ ")") . P.prettyPrintType) tys)
renderDeclaration _ _ = return ()

getName :: P.Declaration -> String
getName (P.TypeDeclaration ident _) = show ident
getName (P.ExternDeclaration _ ident _ _) = show ident
getName (P.DataDeclaration name _ _) = P.runProperName name
getName (P.ExternDataDeclaration name _) = P.runProperName name
getName (P.TypeSynonymDeclaration name _ _) = P.runProperName name
getName (P.TypeClassDeclaration name _ _) = P.runProperName name
getName (P.TypeInstanceDeclaration name _ _ _ _) = show name
getName _ = error "Invalid argument to getName"

isValueDeclaration :: P.Declaration -> Bool
isValueDeclaration P.TypeDeclaration{} = True
isValueDeclaration P.ExternDeclaration{} = True
isValueDeclaration _ = False

isTypeDeclaration :: P.Declaration -> Bool
isTypeDeclaration P.DataDeclaration{} = True
isTypeDeclaration P.ExternDataDeclaration{} = True
isTypeDeclaration P.TypeSynonymDeclaration{} = True
isTypeDeclaration _ = False

isTypeClassDeclaration :: P.Declaration -> Bool
isTypeClassDeclaration P.TypeClassDeclaration{} = True
isTypeClassDeclaration _ = False

isTypeInstanceDeclaration :: P.Declaration -> Bool
isTypeInstanceDeclaration P.TypeInstanceDeclaration{} = True
isTypeInstanceDeclaration _ = False

inputFile :: Term FilePath
inputFile = value $ pos 0 "input.ps" $ posInfo { posDoc = "The input .ps file" }

term :: Term (IO ())
term = docgen <$> inputFile

termInfo :: TermInfo
termInfo = defTI
  { termName = "docgen"
  , version  = showVersion Paths.version
  , termDoc  = "Generate Markdown documentation from PureScript extern files"
  }

main :: IO ()
main = run (term, termInfo)
