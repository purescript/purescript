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

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Function (on)
import Data.List
import Data.Version (showVersion)
import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U
import System.Console.CmdTheLine
import System.Exit (exitSuccess, exitFailure)

docgen :: FilePath -> Bool -> IO ()
docgen input showHierarchy = do
  text <- U.readFile input
  case P.runIndentParser input P.parseModules text of
    Left err -> do
      U.print err
      exitFailure
    Right ms -> do
      U.putStrLn . runDocs $ (renderModules showHierarchy) ms
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

renderModules :: Bool -> [P.Module] -> Docs
renderModules showHierarchy ms = do
  headerLevel 1 "Module Documentation"
  spacer
  mapM_ (renderModule showHierarchy) ms

renderModule :: Bool -> P.Module -> Docs
renderModule showHierarchy (P.Module moduleName ds exps) =
  let exported = filter (isExported exps) ds
      hasTypes = any isTypeDeclaration ds
      hasTypeclasses = any isTypeClassDeclaration ds
      hasTypeclassInstances = any isTypeInstanceDeclaration ds
      hasValues = any isValueDeclaration ds
  in do
    headerLevel 2 $ "Module " ++ P.runModuleName moduleName
    spacer
    when hasTypes $ do
      headerLevel 3 "Types"
      spacer
      renderTopLevel exps (filter isTypeDeclaration exported)
      spacer
    when hasTypeclasses $ do
      headerLevel 3 "Type Classes"
      spacer
      when showHierarchy $ do
        renderTypeclassImage moduleName
        spacer
      renderTopLevel exps (filter isTypeClassDeclaration exported)
      spacer
    when hasTypeclassInstances $ do
      headerLevel 3 "Type Class Instances"
      spacer
      renderTopLevel exps (filter isTypeInstanceDeclaration ds)
      spacer
    when hasValues $ do
      headerLevel 3 "Values"
      spacer
      renderTopLevel exps (filter isValueDeclaration exported)
      spacer

isExported :: Maybe [P.DeclarationRef] -> P.Declaration -> Bool
isExported Nothing _ = True
isExported _ P.TypeInstanceDeclaration{} = True
isExported exps (P.PositionedDeclaration _ d) = isExported exps d
isExported (Just exps) decl = any (matches decl) exps
  where
  matches (P.TypeDeclaration ident _) (P.ValueRef ident') = ident == ident'
  matches (P.ExternDeclaration _ ident _ _) (P.ValueRef ident') = ident == ident'
  matches (P.DataDeclaration ident _ _) (P.TypeRef ident' _) = ident == ident'
  matches (P.ExternDataDeclaration ident _) (P.TypeRef ident' _) = ident == ident'
  matches (P.TypeSynonymDeclaration ident _ _) (P.TypeRef ident' _) = ident == ident'
  matches (P.TypeClassDeclaration ident _ _ _) (P.TypeClassRef ident') = ident == ident'
  matches (P.PositionedDeclaration _ d) r = d `matches` r
  matches d (P.PositionedDeclarationRef _ r) = d `matches` r
  matches _ _ = False

isDctorExported :: P.ProperName -> Maybe [P.DeclarationRef] -> P.ProperName -> Bool
isDctorExported _ Nothing _ = True
isDctorExported ident (Just exps) ctor = flip any exps $ \e -> case e of
  P.TypeRef ident' Nothing -> ident == ident'
  P.TypeRef ident' (Just ctors) -> ident == ident' && ctor `elem` ctors
  _ -> False

renderTopLevel :: Maybe [P.DeclarationRef] -> [P.Declaration] -> Docs
renderTopLevel exps decls = forM_ (sortBy (compare `on` getName) decls) $ \decl -> do
  renderDeclaration 4 exps decl
  spacer

renderTypeclassImage :: P.ModuleName -> Docs
renderTypeclassImage name =
  let name' = P.runModuleName name
  in tell ["![" ++ name' ++ "](images/" ++ name' ++ ".png)"]

renderDeclaration :: Int -> Maybe [P.DeclarationRef] -> P.Declaration -> Docs
renderDeclaration n _ (P.TypeDeclaration ident ty) =
  atIndent n $ show ident ++ " :: " ++ P.prettyPrintType ty
renderDeclaration n _ (P.ExternDeclaration _ ident _ ty) =
  atIndent n $ show ident ++ " :: " ++ P.prettyPrintType ty
renderDeclaration n exps (P.DataDeclaration name args ctors) = do
  let typeName = P.runProperName name ++ " " ++ unwords args
  atIndent n $ "data " ++ typeName ++ " where"
  let exported = filter (isDctorExported name exps . fst) ctors
  forM_ exported $ \(ctor, tys) ->
    atIndent (n + 2) $ P.runProperName ctor ++ " :: " ++ concatMap (\ty -> P.prettyPrintType ty ++ " -> ") tys ++ typeName
renderDeclaration n _ (P.ExternDataDeclaration name kind) =
  atIndent n $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind
renderDeclaration n _ (P.TypeSynonymDeclaration name args ty) = do
  let typeName = P.runProperName name ++ " " ++ unwords args
  atIndent n $ "type " ++ typeName ++ " = " ++ P.prettyPrintType ty
renderDeclaration n exps (P.TypeClassDeclaration name args implies ds) = do
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
  atIndent n $ "class " ++ impliesText ++ P.runProperName name ++ " " ++ unwords args ++ " where"
  mapM_ (renderDeclaration (n + 2) exps) ds
renderDeclaration n _ (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  atIndent n $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)
renderDeclaration n exps (P.PositionedDeclaration _ d) =
  renderDeclaration n exps d
renderDeclaration _ _ _ = return ()

getName :: P.Declaration -> String
getName (P.TypeDeclaration ident _) = show ident
getName (P.ExternDeclaration _ ident _ _) = show ident
getName (P.DataDeclaration name _ _) = P.runProperName name
getName (P.ExternDataDeclaration name _) = P.runProperName name
getName (P.TypeSynonymDeclaration name _ _) = P.runProperName name
getName (P.TypeClassDeclaration name _ _ _) = P.runProperName name
getName (P.TypeInstanceDeclaration name _ _ _ _) = show name
getName (P.PositionedDeclaration _ d) = getName d
getName _ = error "Invalid argument to getName"

isValueDeclaration :: P.Declaration -> Bool
isValueDeclaration P.TypeDeclaration{} = True
isValueDeclaration P.ExternDeclaration{} = True
isValueDeclaration (P.PositionedDeclaration _ d) = isValueDeclaration d
isValueDeclaration _ = False

isTypeDeclaration :: P.Declaration -> Bool
isTypeDeclaration P.DataDeclaration{} = True
isTypeDeclaration P.ExternDataDeclaration{} = True
isTypeDeclaration P.TypeSynonymDeclaration{} = True
isTypeDeclaration (P.PositionedDeclaration _ d) = isTypeDeclaration d
isTypeDeclaration _ = False

isTypeClassDeclaration :: P.Declaration -> Bool
isTypeClassDeclaration P.TypeClassDeclaration{} = True
isTypeClassDeclaration (P.PositionedDeclaration _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

isTypeInstanceDeclaration :: P.Declaration -> Bool
isTypeInstanceDeclaration P.TypeInstanceDeclaration{} = True
isTypeInstanceDeclaration (P.PositionedDeclaration _ d) = isTypeInstanceDeclaration d
isTypeInstanceDeclaration _ = False

inputFile :: Term FilePath
inputFile = value $ pos 0 "input.ps" $ posInfo { posDoc = "The input .ps file" }

includeHeirarcy :: Term Bool
includeHeirarcy = value $ flag $ (optInfo [ "h", "hierarchy-images" ]) { optDoc = "Include markdown for type class hierarchy images in the output." }

term :: Term (IO ())
term = docgen <$> inputFile <*> includeHeirarcy

termInfo :: TermInfo
termInfo = defTI
  { termName = "docgen"
  , version  = showVersion Paths.version
  , termDoc  = "Generate Markdown documentation from PureScript extern files"
  }

main :: IO ()
main = run (term, termInfo)
