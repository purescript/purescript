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

  let (instances, children) = partition ((==) ChildInstance . rcdType) rdChildren
  fencedBlock $ do
    tell' (codeToString rdCode)
    zipWithM_ (\f c -> tell' (childToString f c)) (First : repeat NotFirst) children
  spacer

  unless (null instances) $ do
    headerLevel 5 "Instances"
    fencedBlock $ do
      mapM_ (tell' . childToString NotFirst) instances
    spacer

  for_ rdComments tell'

codeToString :: RenderedCode -> String
codeToString = outputWith elemAsMarkdown
  where
  elemAsMarkdown (Syntax x)  = x
  elemAsMarkdown (Ident x)   = x
  elemAsMarkdown (Ctor x _)  = x
  elemAsMarkdown (Kind x)    = x
  elemAsMarkdown (Keyword x) = x
  elemAsMarkdown Space       = " "

childToString :: First -> RenderedChildDeclaration -> String
childToString f RenderedChildDeclaration{..} =
  case rcdType of
    ChildDataConstructor ->
      let c = if f == First then "=" else "|"
      in  "  " ++ c ++ " " ++ codeToString rcdCode
    ChildTypeClassMember ->
      "  " ++ codeToString rcdCode
    ChildInstance ->
      codeToString rcdCode

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
