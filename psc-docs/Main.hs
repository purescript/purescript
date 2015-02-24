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
import Data.List
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)
import Data.Foldable (traverse_)

import Options.Applicative

import qualified Language.PureScript as P
import qualified Paths_purescript as Paths
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Etags
import Ctags

-- Available output formats
data Format = Markdown -- Output documentation in Markdown format
               | Ctags -- Output ctags symbol index suitable for use with vi
               | Etags -- Output etags symbol index suitable for use with emacs

data PSCDocsOptions = PSCDocsOptions
  { pscdFormat :: Format
  , pscdInputFiles  :: [FilePath]
  }

docgen :: PSCDocsOptions -> IO ()
docgen (PSCDocsOptions fmt input) = do
  e <- P.parseModulesFromFiles (fromMaybe "") <$> mapM (fmap (first Just) . parseFile) (nub input)
  case e of
    Left err -> do
      hPutStrLn stderr $ show err
      exitFailure
    Right ms -> do
      case fmt of
       Markdown -> putStrLn . runDocs $ renderModules (map snd ms)
       Etags -> ldump $ dumpEtags $ pairs ms
       Ctags -> ldump $ dumpCtags $ pairs ms
      exitSuccess
  where pairs :: [(Maybe String, m)] -> [(String, m)]
        pairs = map (\(k,m) -> (fromMaybe "" k,m))
        ldump :: [String] -> IO ()
        ldump = mapM_ putStrLn
    
parseFile :: FilePath -> IO (FilePath, String)
parseFile input = (,) input <$> readFile input

type Docs = Writer [String] ()

runDocs :: Docs -> String
runDocs = unlines . execWriter

spacer :: Docs
spacer = tell [""]

headerLevel :: Int -> String -> Docs
headerLevel level hdr = tell [replicate level '#' ++ ' ' : hdr]

withIndent :: Int -> Docs -> Docs
withIndent indent = censor (map (replicate indent ' ' ++ ))

atIndent :: Int -> String -> Docs
atIndent indent text =
  let ls = lines text in
  withIndent indent (tell ls)

fenced :: String -> Docs
fenced text = fencedBlock (tell $ lines text)

fencedBlock :: Docs -> Docs
fencedBlock inner = do
  tell ["``` purescript"]
  inner
  tell ["```"]

ticks :: String -> String
ticks = ("`" ++) . (++ "`")

renderModules :: [P.Module] -> Docs
renderModules ms = do
  headerLevel 1 "Module Documentation"
  spacer
  mapM_ renderModule ms

renderModule :: P.Module -> Docs
renderModule mdl@(P.Module coms moduleName _ exps) = do
    headerLevel 2 $ "Module " ++ P.runModuleName moduleName
    spacer
    unless (null coms) $ do
      renderComments coms
      spacer
    renderTopLevel exps (P.exportedDeclarations mdl)
    spacer

renderTopLevel :: Maybe [P.DeclarationRef] -> [P.Declaration] -> Docs
renderTopLevel exps decls = forM_ decls $ \decl ->
  when (canRenderDecl decl) $ do
    traverse_ (headerLevel 4) (ticks `fmap` getDeclarationTitle decl)
    spacer
    renderDeclaration exps decl
    spacer

renderTypeclassImage :: P.ModuleName -> Docs
renderTypeclassImage name =
  let name' = P.runModuleName name
  in tell ["![" ++ name' ++ "](images/" ++ name' ++ ".png)"]

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)                      = Just (show name)
getDeclarationTitle (P.ExternDeclaration _ name _ _)                = Just (show name)
getDeclarationTitle (P.DataDeclaration _ name _ _)                  = Just (show name)
getDeclarationTitle (P.ExternDataDeclaration name _)                = Just (show name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)             = Just (show name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)   = Just (show name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _)        = Just (show name)
getDeclarationTitle (P.PositionedDeclaration _ _ d)                 = getDeclarationTitle d
getDeclarationTitle _                                               = Nothing

renderDeclaration :: Maybe [P.DeclarationRef] -> P.Declaration -> Docs
renderDeclaration _ (P.TypeDeclaration ident ty) =
  fenced $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration _ (P.ExternDeclaration _ ident _ ty) =
  fenced $ show ident ++ " :: " ++ prettyPrintType' ty
renderDeclaration exps (P.DataDeclaration dtype name args ctors) = do
  let
    typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    typeName = prettyPrintType' typeApp
    exported = filter (P.isDctorExported name exps . fst) ctors
  fencedBlock $ do
    tell [show dtype ++ " " ++ typeName]
    zipWithM_ (\isFirst (ctor, tys) ->
                atIndent 2 $ (if isFirst then "= " else "| ") ++ P.runProperName ctor ++ " " ++ unwords (map P.prettyPrintTypeAtom tys))
              (True : repeat False) exported
renderDeclaration _ (P.ExternDataDeclaration name kind) =
  fenced $ "data " ++ P.runProperName name ++ " :: " ++ P.prettyPrintKind kind
renderDeclaration _ (P.TypeSynonymDeclaration name args ty) = do
  let
    typeApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
    typeName = prettyPrintType' typeApp
  fenced $ "type " ++ typeName ++ " = " ++ prettyPrintType' ty
renderDeclaration _ (P.TypeClassDeclaration name args implies ds) = do
  let impliesText = case implies of
                      [] -> ""
                      is -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) is) ++ ") <= "
      classApp  = foldl P.TypeApp (P.TypeConstructor (P.Qualified Nothing name)) (map toTypeVar args)
      className = prettyPrintType' classApp
  fencedBlock $ do
    tell ["class " ++ impliesText ++ className ++ " where"]
    mapM_ renderClassMember ds
  where
    renderClassMember (P.PositionedDeclaration _ _ d) = renderClassMember d
    renderClassMember (P.TypeDeclaration ident ty) = atIndent 2 $ show ident ++ " :: " ++ prettyPrintType' ty
    renderClassMember _ = error "Invalid argument to renderClassMember."
renderDeclaration _ (P.TypeInstanceDeclaration name constraints className tys _) = do
  let constraintsText = case constraints of
                          [] -> ""
                          cs -> "(" ++ intercalate ", " (map (\(pn, tys') -> show pn ++ " " ++ unwords (map P.prettyPrintTypeAtom tys')) cs) ++ ") => "
  fenced $ "instance " ++ show name ++ " :: " ++ constraintsText ++ show className ++ " " ++ unwords (map P.prettyPrintTypeAtom tys)
renderDeclaration exps (P.PositionedDeclaration _ com d) = do
  renderDeclaration exps d
  renderComments com
renderDeclaration _ _ = return ()

renderComments :: [P.Comment] -> Docs
renderComments cs = do
  let raw = concatMap toLines cs
  when (all hasPipe raw) $ do
    spacer
    atIndent 0 . unlines . map stripPipes $ raw
  where

  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

  stripPipes = dropPipe . dropWhile (== ' ')

  dropPipe ('|':' ':s) = s
  dropPipe ('|':s) = s
  dropPipe s = s

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

canRenderDecl :: P.Declaration -> Bool
canRenderDecl P.TypeDeclaration{} = True
canRenderDecl P.ExternDeclaration{} = True
canRenderDecl P.DataDeclaration{} = True
canRenderDecl P.ExternDataDeclaration{} = True
canRenderDecl P.TypeSynonymDeclaration{} = True
canRenderDecl P.TypeClassDeclaration{} = True
canRenderDecl P.TypeInstanceDeclaration{} = True
canRenderDecl (P.PositionedDeclaration _ _ d) = canRenderDecl d
canRenderDecl _ = False

inputFile :: Parser FilePath
inputFile = strArgument $
     metavar "FILE"
  <> help "The input .purs file(s)"

instance Read Format where
    readsPrec _ "etags" = [(Etags, "")]
    readsPrec _ "ctags" = [(Ctags, "")]
    readsPrec _ "markdown" = [(Markdown, "")]
    readsPrec _ _ = []    

format :: Parser Format
format = option auto $ value Markdown
         <> long "format"
         <> metavar "FORMAT"
         <> help "Set output FORMAT (markdown | etags | ctags)"  

pscDocsOptions :: Parser PSCDocsOptions
pscDocsOptions = PSCDocsOptions <$> format <*> many inputFile

main :: IO ()
main = execParser opts >>= docgen
  where
  opts        = info (version <*> helper <*> pscDocsOptions) infoModList
  infoModList = fullDesc <> headerInfo <> footerInfo
  headerInfo  = header   "psc-docs - Generate Markdown documentation from PureScript extern files"
  footerInfo  = footer $ "psc-docs " ++ showVersion Paths.version

  version :: Parser (a -> a)
  version = abortOption (InfoMsg (showVersion Paths.version)) $ long "version" <> help "Show the version number" <> hidden
