{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.Docs.Html
  ( asHtml
  , layout
  , writeHtmlModule
  , writeHtmlModules
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&))
import           Control.Monad.Writer
import           Data.List (sort)
import           Data.Text (Text)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T
import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.AsHtml as D
import           Text.Blaze.Html5 (Html, (!), toMarkup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import           System.IO.UTF8 (writeUTF8FileT)
import           System.FilePath.Glob (glob)
import           System.Directory (removeFile)
import           Version (versionString)

writeHtmlModules :: FilePath -> [(P.ModuleName, D.HtmlOutputModule Html)] -> IO ()
writeHtmlModules outputDir modules = do
  glob (outputDir <> "/*.html") >>= mapM_ removeFile
  let moduleList = sort $ map fst modules
  writeHtmlFile (outputDir ++ "/index.html") (renderIndexModule moduleList)
  mapM_ (writeHtmlModule outputDir . (fst &&& layout moduleList)) modules

asHtml :: D.Module -> (P.ModuleName, D.HtmlOutputModule Html)
asHtml m = D.moduleAsHtml (const $ Just $ getHtmlRenderContext (D.modName m)) m

writeHtmlModule :: FilePath -> (P.ModuleName, Html) -> IO ()
writeHtmlModule outputDir (mn, html) = do
  let filepath = outputDir ++ "/" ++ T.unpack (P.runModuleName mn) ++ ".html"
  writeHtmlFile filepath html

writeHtmlFile :: FilePath -> Html -> IO ()
writeHtmlFile filepath =
  writeUTF8FileT filepath . toStrict . Blaze.renderHtml

getHtmlRenderContext :: P.ModuleName -> D.HtmlRenderContext
getHtmlRenderContext mn = D.HtmlRenderContext
  { D.currentModuleName = mn
  , D.buildDocLink = getLink mn
  , D.renderDocLink = renderLink
  , D.renderSourceLink = const Nothing
  }

-- TODO: try to combine this with the one in Docs.Types?
getLink :: P.ModuleName -> D.Namespace -> Text -> D.ContainingModule -> Maybe D.DocLink
getLink curMn namespace target containingMod = do
  location <- getLinkLocation
  return D.DocLink
    { D.linkLocation = location
    , D.linkTitle = target
    , D.linkNamespace = namespace
    }

  where
  getLinkLocation = builtinLinkLocation <|> normalLinkLocation

  normalLinkLocation = do
    case containingMod of
      D.ThisModule ->
        return D.SameModule
      D.OtherModule destMn ->
        -- This is OK because all modules count as 'local' for purs docs in
        -- html mode
        return $ D.LocalModule curMn destMn

  builtinLinkLocation = do
    let primMn = P.moduleNameFromString "Prim"
    guard $ containingMod == D.OtherModule primMn
    return $ D.BuiltinModule primMn

renderLink :: D.DocLink -> Text
renderLink l =
  case D.linkLocation l of
    D.SameModule ->
      ""
    D.LocalModule _ dest ->
      P.runModuleName dest <> ".html"
    D.DepsModule{} ->
      P.internalError "DepsModule: not implemented"
    D.BuiltinModule dest  ->
      P.runModuleName dest <> ".html"

layout :: [P.ModuleName] -> (P.ModuleName, D.HtmlOutputModule Html) -> Html
layout moduleList (mn, htmlDocs) =
  basicLayout ("PureScript: " <> modName) $ do
    H.div ! A.class_ "page-title clearfix" $ do
      H.div ! A.class_ "page-title__label" $ "Module"
      H.h1 ! A.class_ "page-title__title" $ toMarkup modName

    H.div ! A.class_ "col col--main" $ do
      D.htmlOutputModuleLocals htmlDocs
      mapM_ renderReExports (D.htmlOutputModuleReExports htmlDocs)

    H.div ! A.class_ "col col--aside" $ do
      H.h3 "Modules"
      renderModuleList moduleList
  where
  modName = P.runModuleName mn

  renderReExports :: (D.InPackage P.ModuleName, Html) -> Html
  renderReExports (reExpFrom, html) = do
    H.h2 ! A.class_ "re-exports" $ do
      toMarkup ("Re-exports from " :: Text)
      H.a ! A.href (H.toValue (toText reExpFrom <> ".html")) $
        toMarkup (toText reExpFrom)
    html

  toText = P.runModuleName . D.ignorePackage

basicLayout :: Text -> Html -> Html
basicLayout title inner =
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title (toMarkup title)

      H.link ! A.href "https://fonts.googleapis.com/css?family=Roboto+Mono|Roboto:300,400,400i,700,700i"
             ! A.type_ "text/css" ! A.rel "stylesheet"
      H.style ! A.type_ "text/css" $
        toMarkup D.normalizeCssT
      H.style ! A.type_ "text/css" $
        toMarkup D.pursuitCssT
    H.body $ do
      H.div ! A.class_ "everything-except-footer" $ do
        H.div ! A.class_ "top-banner clearfix" $ do
          H.div ! A.class_ "container clearfix" $ do
            H.div ! A.style inlineHeaderStyles $ do
              "PureScript API documentation"

            H.div ! A.class_ "top-banner__actions" $ do
              H.div ! A.class_ "top-banner__actions__item" $ do
                H.a ! A.href "index.html" $ "Index"

        H.main ! A.class_ "container clearfix" ! H.customAttribute "role" "main" $ do
          inner

      H.div ! A.class_ "footer clearfix" $
        H.p $ toMarkup $ "Generated by purs " <> versionString

  where
  -- Like Pursuit's .top-banner__logo except without the 'hover' styles
  inlineHeaderStyles = "float: left; font-size: 2.44em; font-weight: 300; line-height: 90px; margin: 0"

renderIndexModule :: [P.ModuleName] -> Html
renderIndexModule moduleList =
  basicLayout "PureScript API documentation" $ do
    H.div ! A.class_ "page-title clearfix" $ do
      H.h1 ! A.class_ "page-title__title" $ "Index"
    H.div ! A.class_ "col col--main" $ do
      renderModuleList moduleList

renderModuleList :: [P.ModuleName] -> Html
renderModuleList moduleList =
  H.ul $ mapM_ listItem moduleList

  where
  listItem mn = H.li $
    H.a ! A.href (H.toValue (P.runModuleName mn <> ".html")) $
      toMarkup (P.runModuleName mn)
