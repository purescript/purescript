{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module Language.PureScript.Docs.AsHtml (
  HtmlOutput(..),
  packageAsHtml
) where

import Control.Arrow (second)
import Control.Category ((>>>))
import Data.Char (toUpper)
import Data.Ord (comparing)
import Data.Monoid (mconcat)
import Data.Foldable (for_)
import Data.List (intercalate, find, sortBy)
import qualified Data.DList as DList
import Data.List.Split (splitOn)
import Data.Default (def)
import Data.Version
import qualified Data.Map as M

import qualified Data.Text as T

import Lucid hiding (for_)
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html.Renderer.Text as H
import qualified Cheapskate

import System.FilePath ((</>))

import Web.Bower.PackageMeta hiding (Version)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)
import Language.PureScript.Docs.Utils.HtmlHelpers

data LinksContext = LinksContext
  { ctxGithub               :: (GithubUser, GithubRepo)
  , ctxBookmarks            :: [Bookmark]
  , ctxResolvedDependencies :: [(PackageName, Version)]
  }
  deriving (Show, Eq, Ord)

-- | A LinksContext with the current module name.
type LinksContext' = (LinksContext, P.ModuleName)

data DocLink
  -- | A link to a declaration in the same module; in this case, only the title
  -- is needed to generate the link.
  = SameModule String

  -- | A link to a declaration in a different module, but still in the current
  -- package; we need to store the current module, the other declaration's
  -- module, and the declaration title.
  | LocalModule P.ModuleName P.ModuleName String

  -- | A link to a declaration in a different package. We store: current module
  -- name, name of the other package, version of the other package, name of
  -- the module in the other package that the declaration is in, and
  -- declaration title.
  | DepsModule P.ModuleName PackageName Version P.ModuleName String
  deriving (Show, Eq, Ord)

data HtmlOutput = HtmlOutput
  { htmlIndex     :: [(Maybe Char, Html ())]
  , htmlModules   :: [(P.ModuleName, Html ())]
  }
  deriving (Show)

packageAsHtml :: UploadedPackage -> HtmlOutput
packageAsHtml UploadedPackage{..} = HtmlOutput indexFile modules
  where
  indexFile = renderIndex ctx
  modules = map (moduleAsHtml ctx) pkgModules
  ctx = LinksContext pkgGithub pkgBookmarks pkgResolvedDependencies

moduleAsHtml :: LinksContext -> RenderedModule -> (P.ModuleName, Html ())
moduleAsHtml ctx RenderedModule{..} = (mn, html)
  where
  mn = P.moduleNameFromString rmName
  ctx' = (ctx, mn)
  html = do
    h1_ $ do
      text "module "
      strong_ (text rmName)
    for_ rmComments renderComments
    for_ rmDeclarations (declAsHtml ctx')

renderIndex :: LinksContext -> [(Maybe Char, Html ())]
renderIndex LinksContext{..} = go ctxBookmarks
  where
  go = takeLocals
     >>> groupIndex getIndex renderEntry
     >>> map (second (ul_ . mconcat))

  getIndex (_, ((toUpper -> c) :_))
    | c `elem` ['A'..'Z'] = Just c
    | otherwise = Nothing
  getIndex _ = Nothing

  renderEntry (mn, title) =
    li_ $ do
      let url = T.pack ((filePathFor mn `relativeTo` "index") ++ "#" ++ title)
      code_ (a_ [href_ url] (text title))
      sp
      text ("(" ++ show mn ++ ")")

  groupIndex :: Ord i => (a -> Maybe i) -> (a -> b) -> [a] -> [(Maybe i, [b])]
  groupIndex f g =
    map (second DList.toList) . M.toList . foldr go' M.empty . sortBy (comparing f)
    where
    go' x = insertOrAppend (f x) (g x)
    insertOrAppend idx val m =
      let cur = M.findWithDefault DList.empty idx m
          new = DList.snoc cur val
      in  M.insert idx new m

declAsHtml :: LinksContext' -> RenderedDeclaration -> Html ()
declAsHtml ctx RenderedDeclaration{..} = do
  a_ [name_ (T.pack rdTitle), href_ (T.pack ('#' : rdTitle))] $
    h2_ (code_ (text rdTitle))
  para "decl" (code_ (codeAsHtml ctx rdCode))
  renderChildren ctx rdChildren
  case rdComments of
    Just cs -> renderComments cs
    Nothing -> return ()
  for_ rdSourceSpan (linkToSource ctx)

renderChildren :: LinksContext' -> [RenderedChildDeclaration] -> Html ()
renderChildren _   [] = return ()
renderChildren ctx xs = go xs
  where
  go = ul_ . mapM_ (li_ . code_ . codeAsHtml ctx . rcdCode)

codeAsHtml :: LinksContext' -> RenderedCode -> Html ()
codeAsHtml ctx = outputWith elemAsHtml
  where
  elemAsHtml (Syntax x)  = withClass "syntax" (text x)
  elemAsHtml (Ident x)   = withClass "ident" (text x)
  elemAsHtml (Ctor x mn) = linkToConstructor ctx x mn (withClass "ctor" (text x))
  elemAsHtml (Kind x)    = text x
  elemAsHtml (Keyword x) = withClass "keyword" (text x)
  elemAsHtml Space       = text " "

getLink :: LinksContext' -> String -> ContainingModule -> Maybe DocLink
getLink (LinksContext{..}, curMn) ctor' containingMod = do
  let bookmark' = (fromContainingModule curMn containingMod, ctor')
  bookmark <- find ((bookmark' ==) . ignorePackage) ctxBookmarks

  case containingMod of
    ThisModule -> return (SameModule ctor')
    OtherModule destMn ->
      case bookmark of
        Local _ -> return (LocalModule curMn destMn ctor')
        FromDep pkgName _ -> do
          pkgVersion <- lookup pkgName ctxResolvedDependencies
          return (DepsModule curMn pkgName pkgVersion destMn ctor')

renderLink :: DocLink -> Html () -> Html ()
renderLink (SameModule x) = linkTo ('#' : x)
renderLink (LocalModule srcMn destMn x) =
  let uri = filePathFor destMn `relativeTo` filePathFor srcMn
  in  linkTo (uri ++ "#" ++ x)
renderLink (DepsModule srcMn pkgName pkgVersion destMn x) =
  let relativeTo' = relativeToOtherPackage pkgName pkgVersion
      uri = filePathFor destMn `relativeTo'` filePathFor srcMn
  in  linkTo (uri ++ "#" ++ x)

linkToConstructor :: LinksContext' -> String -> ContainingModule -> Html () -> Html ()
linkToConstructor ctx ctor' containMn =
  maybe id renderLink (getLink ctx ctor' containMn)

linkToSource :: LinksContext' -> P.SourceSpan -> Html ()
linkToSource (LinksContext{..}, _) (P.SourceSpan name start end) =
  linkTo (concat
            [githubBaseUrl, "/tree/master/", relativeToBase name, "#", fragment])
         (text "Source")
  where
  (P.SourcePos startLine _) = start
  (P.SourcePos endLine _) = end
  (GithubUser user, GithubRepo repo) = ctxGithub

  relativeToBase = intercalate "/" . dropWhile (/= "src") . splitOn "/"
  githubBaseUrl = concat ["https://github.com/", user, "/", repo]
  fragment = "L" ++ show startLine ++ "-L" ++ show endLine

-- TODO: use GitHub API instead?
renderComments :: String -> Html ()
renderComments = toHtml . H.renderHtml . H.toHtml . Cheapskate.markdown def . T.pack

-- | if `to` and `from` are both files in the current package, generate a
-- FilePath for `to` relative to `from`.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo to from = go (splitOn "/" to) (splitOn "/" from)
  where
  go (x : xs) (y : ys) | x == y = go xs ys
  go xs ys = intercalate "/" $ replicate (length ys - 1) ".." ++ xs

-- | Generate a FilePath for module documentation for a module in the current
-- package.
filePathFor :: P.ModuleName -> FilePath
filePathFor (P.ModuleName parts) = go parts
  where
  go [] = "index.html"
  go (x : xs) = show x </> go xs

-- | Like `relativeTo`, but in the case where `to` is in another package.
relativeToOtherPackage :: PackageName -> Version -> FilePath -> FilePath -> FilePath
relativeToOtherPackage name (showVersion -> vers) to from =
  intercalate "/" (dots ++ [runPackageName name, vers] ++ splitOn "/" to)
  where
  dots = replicate (length from - 1) ".."
