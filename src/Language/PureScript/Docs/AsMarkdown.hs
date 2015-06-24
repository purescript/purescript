{-# LANGUAGE RecordWildCards #-}

module Language.PureScript.Docs.AsMarkdown (
  renderModulesAsMarkdown
) where

import Control.Monad.Writer hiding (First)
import Data.Foldable (for_)
import Data.List (partition)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode
import qualified Language.PureScript.Docs.Render as Render

-- |
-- Take a list of modules and render them all in order, returning a single
-- Markdown-formatted String.
--
renderModulesAsMarkdown :: [P.Module] -> String
renderModulesAsMarkdown =
  runDocs . modulesAsMarkdown . map Render.renderModule

modulesAsMarkdown :: [RenderedModule] -> Docs
modulesAsMarkdown = mapM_ moduleAsMarkdown

moduleAsMarkdown :: RenderedModule -> Docs
moduleAsMarkdown RenderedModule{..} = do
  headerLevel 2 $ "Module " ++ rmName
  spacer
  for_ rmComments tell'
  mapM_ declAsMarkdown rmDeclarations
  spacer

declAsMarkdown :: RenderedDeclaration -> Docs
declAsMarkdown RenderedDeclaration{..} = do
  headerLevel 4 (ticks rdTitle)
  spacer

  let (instances, children) = partition (isChildInstance . rcdInfo) rdChildren
  fencedBlock $ do
    tell' (codeToString rdCode)
    zipWithM_ (\f c -> tell' (childToString f c)) (First : repeat NotFirst) children
  spacer

  for_ rdFixity (\fixity -> fixityAsMarkdown fixity >> spacer)

  for_ rdComments tell'

  unless (null instances) $ do
    headerLevel 5 "Instances"
    fencedBlock $ mapM_ (tell' . childToString NotFirst) instances
    spacer

  where
  isChildInstance (ChildInstance _) = True
  isChildInstance _ = False

codeToString :: RenderedCode -> String
codeToString = outputWith elemAsMarkdown
  where
  elemAsMarkdown (Syntax x)  = x
  elemAsMarkdown (Ident x)   = x
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

childToString :: First -> RenderedChildDeclaration -> String
childToString f RenderedChildDeclaration{..} =
  case rcdInfo of
    ChildDataConstructor sig _ ->
      let c = if f == First then "=" else "|"
      in  "  " ++ c ++ " " ++ codeToString sig
    ChildTypeClassMember ty _ ->
      "  " ++ codeToString ty
    ChildInstance code ->
      codeToString code

data First
  = First
  | NotFirst
  deriving (Show, Eq, Ord)

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
