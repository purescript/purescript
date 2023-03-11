module Command.Docs.Html
  ( asHtml
  , layout
  , writeHtmlModule
  , writeHtmlModules
  ) where

import Prelude

import Control.Applicative ( Alternative((<|>)) )
import Control.Arrow ((&&&))
import Control.Monad.Writer ( guard )
import Data.List (sort)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text qualified as T
import Language.PureScript.Crash ( internalError )
import Language.PureScript.Names qualified as PN
import Language.PureScript.Docs.Css ( normalizeCssT, pursuitCssT )
import Language.PureScript.Docs.RenderedCode.Types ( ContainingModule(..), Namespace )
import Language.PureScript.Docs.Types ( ignorePackage, DocLink(..), InPackage, LinkLocation(BuiltinModule, LocalModule, DepsModule), Module(modName) )
import Language.PureScript.Docs.AsHtml qualified as DHtml
import Text.Blaze.Html5 (Html, (!), toMarkup)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import System.IO.UTF8 (writeUTF8FileT)
import Version (versionString)

writeHtmlModules :: FilePath -> [(PN.ModuleName, DHtml.HtmlOutputModule Html)] -> IO ()
writeHtmlModules outputDir modules = do
  let moduleList = sort $ map fst modules
  writeHtmlFile (outputDir ++ "/index.html") (renderIndexModule moduleList)
  mapM_ (writeHtmlModule outputDir . (fst &&& layout moduleList)) modules

asHtml :: Module -> (PN.ModuleName, DHtml.HtmlOutputModule Html)
asHtml m = DHtml.moduleAsHtml (const $ Just $ getHtmlRenderContext (modName m)) m

writeHtmlModule :: FilePath -> (PN.ModuleName, Html) -> IO ()
writeHtmlModule outputDir (mn, html) = do
  let filepath = outputDir ++ "/" ++ T.unpack (PN.runModuleName mn) ++ ".html"
  writeHtmlFile filepath html

writeHtmlFile :: FilePath -> Html -> IO ()
writeHtmlFile filepath =
  writeUTF8FileT filepath . toStrict . Blaze.renderHtml

getHtmlRenderContext :: PN.ModuleName -> DHtml.HtmlRenderContext
getHtmlRenderContext mn = DHtml.HtmlRenderContext
  { DHtml.buildDocLink = getLink mn
  , DHtml.renderDocLink = renderLink
  , DHtml.renderSourceLink = const Nothing
  }

-- TODO: try to combine this with the one in Docs.Types?
getLink :: PN.ModuleName -> Namespace -> Text -> ContainingModule -> Maybe DocLink
getLink curMn namespace target containingMod = do
  location <- getLinkLocation
  return DocLink
    { linkLocation = location
    , linkTitle = target
    , linkNamespace = namespace
    }

  where
  getLinkLocation = builtinLinkLocation <|> normalLinkLocation

  normalLinkLocation = do
    case containingMod of
      ThisModule ->
        return $ LocalModule curMn
      OtherModule destMn ->
        -- This is OK because all modules count as 'local' for purs docs in
        -- html mode
        return $ LocalModule destMn

  builtinLinkLocation = do
    let primMn = PN.moduleNameFromString "Prim"
    guard $ containingMod == OtherModule primMn
    return $ BuiltinModule primMn

renderLink :: DocLink -> Text
renderLink l =
  case linkLocation l of
    LocalModule dest ->
      PN.runModuleName dest <> ".html"
    DepsModule{} ->
      internalError "DepsModule: not implemented"
    BuiltinModule dest  ->
      PN.runModuleName dest <> ".html"

layout :: [PN.ModuleName] -> (PN.ModuleName, DHtml.HtmlOutputModule Html) -> Html
layout moduleList (mn, htmlDocs) =
  basicLayout ("PureScript: " <> modName) $ do
    H.div ! A.class_ "page-title clearfix" $ do
      H.div ! A.class_ "page-title__label" $ "Module"
      H.h1 ! A.class_ "page-title__title" $ toMarkup modName

    H.div ! A.class_ "col col--main" $ do
      DHtml.htmlOutputModuleLocals htmlDocs
      mapM_ renderReExports (DHtml.htmlOutputModuleReExports htmlDocs)

    H.div ! A.class_ "col col--aside" $ do
      H.h3 "Modules"
      renderModuleList moduleList
  where
  modName = PN.runModuleName mn

  renderReExports :: (InPackage PN.ModuleName, Html) -> Html
  renderReExports (reExpFrom, html) = do
    H.h2 ! A.class_ "re-exports" $ do
      toMarkup ("Re-exports from " :: Text)
      H.a ! A.href (H.toValue (toText reExpFrom <> ".html")) $
        toMarkup (toText reExpFrom)
    html

  toText = PN.runModuleName . ignorePackage

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
        toMarkup normalizeCssT
      H.style ! A.type_ "text/css" $
        toMarkup pursuitCssT
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

renderIndexModule :: [PN.ModuleName] -> Html
renderIndexModule moduleList =
  basicLayout "PureScript API documentation" $ do
    H.div ! A.class_ "page-title clearfix" $ do
      H.h1 ! A.class_ "page-title__title" $ "Index"
    H.div ! A.class_ "col col--main" $ do
      renderModuleList moduleList

renderModuleList :: [PN.ModuleName] -> Html
renderModuleList moduleList =
  H.ul $ mapM_ listItem moduleList

  where
  listItem mn = H.li $
    H.a ! A.href (H.toValue (PN.runModuleName mn <> ".html")) $
      toMarkup (PN.runModuleName mn)
