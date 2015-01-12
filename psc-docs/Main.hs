----------------------------------------------------------------------------
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
import Control.Arrow (first)
import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import Options.Applicative

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import qualified System.IO.UTF8 as U
import System.Exit (exitSuccess, exitFailure)
import System.IO (stderr)


data PSCDocsOptions = PSCDocsOptions
  { pscdIncludeHeir :: Bool
  , pscdInputFiles  :: [FilePath]
  }


docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions showHierarchy input) = do
  e <- P.parseModulesFromFiles (fromMaybe "") <$> mapM (fmap (first Just) . parseFile) (nub input)
  case e of
    Left err -> do
      U.hPutStr stderr $ show err
      exitFailure
    Right ms -> do
      U.putStrLn . runDocs $ (renderModules showHierarchy) (map snd ms)
      exitSuccess

parseFile :: FilePath -> IO (FilePath, String)
parseFile input = (,) input <$> U.readFile input

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
renderModule showHierarchy mdl@(P.Module moduleName _ exps) =
  let ds = P.exportedDeclarations mdl
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
      renderTopLevel exps (filter isTypeDeclaration ds)
      spacer
    when hasTypeclasses $ do
      headerLevel 3 "Type Classes"
      spacer
      when showHierarchy $ do
        renderTypeclassImage moduleName
        spacer
      renderTopLevel exps (filter isTypeClassDeclaration ds)
      spacer
    when hasTypeclassInstances $ do
      headerLevel 3 "Type Class Instances"
      spacer
      renderTopLevel exps (filter isTypeInstanceDeclaration ds)
      spacer
    when hasValues $ do
      headerLevel 3 "Values"
      spacer
      renderTopLevel exps (filter isValueDeclaration ds)
      spacer

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
  atIndent n $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration n _ (P.ExternDeclaration _ ident _ ty) =
  atIndent n $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration n exps (P.DataDeclaration dtype name args ctors) = do
  let
    typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    typeName = prettyPrintType' typeApp
    exported = filter (P.isDctorExported name exps . fst) ctors
  atIndent n $ show dtype ++ " " ++ typeName ++ (if null exported then "" else " where")
  forM_ exported $ \(ctor, tys) ->
    let ctorTy = foldr P.function typeApp tys
    in atIndent (n + 2) $ P.runProperName ctor ++ " :: " ++ prettyPrintType' ctorTy
renderDeclaration n _ (P.ExternDataDeclaration name kind) =
  atIndent n $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind
renderDeclaration n _ (P.TypeSynonymDeclaration name args ty) = do
  let
    typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    typeName = prettyPrintType' typeApp    
  atIndent n $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty
renderDeclaration n exps (P.TypeClassDeclaration name args implies ds) = do
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
      classApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
      className = prettyPrintType' classApp
  atIndent n $ "class " ++ impliesText ++ className ++ " where"
  mapM_ (renderDeclaration (n + 2) exps) ds
renderDeclaration n _ (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  atIndent n $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)
renderDeclaration n exps (P.PositionedDeclaration _ com d) = do
  renderComments n com
  spacer
  renderDeclaration n exps d
renderDeclaration _ _ _ = return ()

renderComments :: Int -> [P.Comment] -> Docs
renderComments n cs = mapM_ (atIndent n) ls
  where
  ls = concatMap toLines cs
  
  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

toTypeVar :: (String, Maybe P.Kind) -> P.Type
toTypeVar (s, Nothing) = P.TypeVar s
toTypeVar (s, Just k) = P.KindedType (P.TypeVar s) k

prettyPrintType' :: P.Type -> String
prettyPrintType' = P.prettyPrintType . P.everywhereOnTypes dePrim
  where
  dePrim ty@(P.TypeConstructor (P.Qualified _ name))
    | ty == P.tyBoolean || ty == P.tyNumber || ty == P.tyString =
      P.TypeConstructor $ P.Qualified Nothing name
  dePrim other = other

getName :: P.Declaration -> String
getName (P.TypeDeclaration ident _) = show ident
getName (P.ExternDeclaration _ ident _ _) = show ident
getName (P.DataDeclaration _ name _ _) = P.runProperName name
getName (P.ExternDataDeclaration name _) = P.runProperName name
getName (P.TypeSynonymDeclaration name _ _) = P.runProperName name
getName (P.TypeClassDeclaration name _ _ _) = P.runProperName name
getName (P.TypeInstanceDeclaration name _ _ _ _) = show name
getName (P.PositionedDeclaration _ _ d) = getName d
getName _ = error "Invalid argument to getName"

isValueDeclaration :: P.Declaration -> Bool
isValueDeclaration P.TypeDeclaration{} = True
isValueDeclaration P.ExternDeclaration{} = True
isValueDeclaration (P.PositionedDeclaration _ _ d) = isValueDeclaration d
isValueDeclaration _ = False

isTypeDeclaration :: P.Declaration -> Bool
isTypeDeclaration P.DataDeclaration{} = True
isTypeDeclaration P.ExternDataDeclaration{} = True
isTypeDeclaration P.TypeSynonymDeclaration{} = True
isTypeDeclaration (P.PositionedDeclaration _ _ d) = isTypeDeclaration d
isTypeDeclaration _ = False

isTypeClassDeclaration :: P.Declaration -> Bool
isTypeClassDeclaration P.TypeClassDeclaration{} = True
isTypeClassDeclaration (P.PositionedDeclaration _ _ d) = isTypeClassDeclaration d
isTypeClassDeclaration _ = False

isTypeInstanceDeclaration :: P.Declaration -> Bool
isTypeInstanceDeclaration P.TypeInstanceDeclaration{} = True
isTypeInstanceDeclaration (P.PositionedDeclaration _ _ d) = isTypeInstanceDeclaration d
isTypeInstanceDeclaration _ = False

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

includeHeirarcy :: Parser Bool
includeHeirarcy = switch $ 
     long "hierarchy-images"
  <> help "Include markdown for type class hierarchy images in the output."

pscDocsOptions :: Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> includeHeirarcy
                                <*> many inputFile

main :: IO ()
main = execParser opts >>= docgen
  where
  opts        = info (version <*> helper <*> pscDocsOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-docs - Generate Markdown documentation from PureScript extern files"
  footerInfo  = footer $ "psc-docs " ++ showVersion Paths.version
  
  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
