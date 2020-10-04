module Language.PureScript.Docs.AsMarkdown
  ( Docs
  , runDocs
  , modulesAsMarkdown
  , moduleAsMarkdown
  , codeToString
  ) where

import Prelude.Compat

import Control.Monad (unless, zipWithM_)
import Control.Monad.Writer (Writer, tell, execWriter)

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Docs.RenderedCode
import Language.PureScript.Docs.Types
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs.Render as Render

modulesAsMarkdown :: [Module] -> Docs
modulesAsMarkdown = mapM_ moduleAsMarkdown

moduleAsMarkdown :: Module -> Docs
moduleAsMarkdown Module{..} = do
  headerLevel 2 $ "Module " <> P.runModuleName modName
  spacer
  for_ modComments tell'
  mapM_ declAsMarkdown modDeclarations
  spacer
  for_ modReExports $ \(mn', decls) -> do
    let mn = ignorePackage mn'
    headerLevel 3 $ "Re-exported from " <> P.runModuleName mn <> ":"
    spacer
    mapM_ declAsMarkdown decls

declAsMarkdown :: Declaration -> Docs
declAsMarkdown decl@Declaration{..} = do
  headerLevel 4 (ticks declTitle)
  spacer

  let (instances, children) = partition (isChildInstance . cdeclInfo) declChildren
  fencedBlock $ do
    tell' (codeToString $ Render.renderDeclaration decl)
    zipWithM_ (\f c -> childToString f c) (First : repeat NotFirst) children
  spacer

  for_ declComments tell'

  unless (null instances) $ do
    headerLevel 5 "Instances"
    mapM_ (childToString NotFirst) instances
    spacer

  where
  isChildInstance (ChildInstanceChain _ ) = True
  isChildInstance (ChildPartOfInstanceChain _) = True
  isChildInstance _ = False

codeToString :: RenderedCode -> Text
codeToString = outputWith elemAsMarkdown
  where
  elemAsMarkdown (Syntax x)     = x
  elemAsMarkdown (Keyword x)    = x
  elemAsMarkdown Space          = " "
  elemAsMarkdown (Symbol _ x _) = x

-- fixityAsMarkdown :: P.Fixity -> Docs
-- fixityAsMarkdown (P.Fixity associativity precedence) =
--   tell' $ concat [ "_"
--                  , associativityStr
--                  , " / precedence "
--                  , show precedence
--                  , "_"
--                  ]
--   where
--   associativityStr = case associativity of
--     P.Infixl -> "left-associative"
--     P.Infixr -> "right-associative"
--     P.Infix  -> "non-associative"

childToString :: First -> ChildDeclaration -> Docs
childToString f decl@ChildDeclaration{..} =
  case cdeclInfo of
    ChildDataConstructor _ ->
      let c = if f == First then "=" else "|"
      in  fencedBlock $ do
        tell' $ "  " <> c <> " " 
        str
    ChildTypeClassMember _ ->
      fencedBlock $ do
        tell' $ "  " 
        str
    ChildInstanceChain  _ ->
      str
    ChildPartOfInstanceChain _ ->
      fencedBlock $ str
  where
  str = case  Render.renderChildDeclaration decl of 
      Render.RenderedAsCode code -> tell' $ codeToString code
      Render.RenderedAsStructure structure -> mapM_ chainInstanceToString structure

  chainInstanceToString :: (ChildInstanceChainInfo, RenderedCode) -> Docs
  chainInstanceToString (inst, code) = do
    fencedBlock $ tell' $ codeToString code
    mapM_ tell' $ icComments inst

data First
  = First
  | NotFirst
  deriving (Show, Eq, Ord)

type Docs = Writer [Text] ()

runDocs :: Docs -> Text
runDocs = T.unlines . execWriter

tell' :: Text -> Docs
tell' = tell . (:[])

spacer :: Docs
spacer = tell' ""

headerLevel :: Int -> Text -> Docs
headerLevel level hdr = tell' (T.replicate level "#" <> " " <> hdr)

fencedBlock :: Docs -> Docs
fencedBlock inner = do
  tell' "``` purescript"
  inner
  tell' "```"

ticks :: Text -> Text
ticks = ("`" <>) . (<> "`")
