module Language.PureScript.Docs.AsMarkdown
  ( renderModulesAsMarkdown
  , Docs
  , runDocs
  , modulesAsMarkdown
  , codeToString
  ) where

import Prelude.Compat

import Control.Monad (unless, zipWithM_)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Writer (Writer, tell, execWriter)

import Data.Foldable (for_)
import Data.List (partition)

import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs.Convert as Convert
import qualified Language.PureScript.Docs.Render as Render

-- |
-- Take a list of modules and render them all in order, returning a single
-- Markdown-formatted String.
--
renderModulesAsMarkdown ::
  (MonadError P.MultipleErrors m) =>
  [P.Module] ->
  m String
renderModulesAsMarkdown =
  fmap (runDocs . modulesAsMarkdown) . Convert.convertModules

modulesAsMarkdown :: [Module] -> Docs
modulesAsMarkdown = mapM_ moduleAsMarkdown

moduleAsMarkdown :: Module -> Docs
moduleAsMarkdown Module{..} = do
  headerLevel 2 $ "Module " ++ P.runModuleName modName
  spacer
  for_ modComments tell'
  mapM_ (declAsMarkdown modName) modDeclarations
  spacer
  for_ modReExports $ \(mn, decls) -> do
    headerLevel 3 $ "Re-exported from " ++ P.runModuleName mn ++ ":"
    spacer
    mapM_ (declAsMarkdown mn) decls

declAsMarkdown :: P.ModuleName -> Declaration -> Docs
declAsMarkdown mn decl@Declaration{..} = do
  let options = defaultRenderTypeOptions { currentModule = Just mn }
  headerLevel 4 (ticks declTitle)
  spacer

  let (instances, children) = partition (isChildInstance . cdeclInfo) declChildren
  fencedBlock $ do
    tell' (codeToString $ Render.renderDeclarationWithOptions options decl)
    zipWithM_ (\f c -> tell' (childToString f c)) (First : repeat NotFirst) children
  spacer

  for_ declFixity (\fixity -> fixityAsMarkdown fixity >> spacer)

  for_ declComments tell'

  unless (null instances) $ do
    headerLevel 5 "Instances"
    fencedBlock $ mapM_ (tell' . childToString NotFirst) instances
    spacer

  where
  isChildInstance (ChildInstance _ _) = True
  isChildInstance _ = False

codeToString :: RenderedCode -> String
codeToString = outputWith elemAsMarkdown
  where
  elemAsMarkdown (Syntax x)  = x
  elemAsMarkdown (Ident x _) = x
  elemAsMarkdown (Ctor x _)  = x
  elemAsMarkdown (Kind x)    = x
  elemAsMarkdown (Keyword x) = x
  elemAsMarkdown Space       = " "

fixityAsMarkdown :: P.Fixity -> Docs
fixityAsMarkdown (P.Fixity associativity precedence) =
  tell' $ concat [ "_"
                 , associativityStr
                 , " / precedence "
                 , show precedence
                 , "_"
                 ]
  where
  associativityStr = case associativity of
    P.Infixl -> "left-associative"
    P.Infixr -> "right-associative"
    P.Infix  -> "non-associative"

childToString :: First -> ChildDeclaration -> String
childToString f decl@ChildDeclaration{..} =
  case cdeclInfo of
    ChildDataConstructor _ ->
      let c = if f == First then "=" else "|"
      in  "  " ++ c ++ " " ++ str
    ChildTypeClassMember _ ->
      "  " ++ str
    ChildInstance _ _ ->
      str
  where
  str = codeToString $ Render.renderChildDeclaration decl

data First
  = First
  | NotFirst
  deriving (Show, Read, Eq, Ord)

type Docs = Writer [String] ()

runDocs :: Docs -> String
runDocs = unlines . execWriter

tell' :: String -> Docs
tell' = tell . (:[])

spacer :: Docs
spacer = tell' ""

headerLevel :: Int -> String -> Docs
headerLevel level hdr = tell' (replicate level '#' ++ ' ' : hdr)

fencedBlock :: Docs -> Docs
fencedBlock inner = do
  tell' "``` purescript"
  inner
  tell' "```"

ticks :: String -> String
ticks = ("`" ++) . (++ "`")
