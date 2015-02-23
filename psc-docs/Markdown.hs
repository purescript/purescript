module Markdown (dumpMarkdown) where

import Control.Monad
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Foldable (traverse_)

import qualified Language.PureScript as P

type Docs = Writer [String] ()

dumpMarkdown :: [P.Module] -> IO ()
dumpMarkdown ms = putStrLn $ runDocs $ renderModules ms

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

