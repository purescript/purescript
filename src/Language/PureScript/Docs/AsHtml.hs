
-- | Functions for rendering generated documentation from PureScript code as
-- HTML.

module Language.PureScript.Docs.AsHtml (
  HtmlOutput(..),
  HtmlOutputModule(..),
  HtmlRenderContext(..),
  nullRenderContext,
  declNamespace,
  packageAsHtml,
  moduleAsHtml,
  makeFragment,
  renderMarkdown
) where

import Prelude
import Control.Category ((>>>))
import Control.Monad (unless)
import Data.Char (isUpper)
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Foldable (for_)
import Data.String (fromString)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import qualified Cheapskate
import Text.Parsec (eof)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types
import Language.PureScript.Docs.RenderedCode hiding (sp)
import qualified Language.PureScript.Docs.Render as Render

declNamespace :: Declaration -> Namespace
declNamespace = declInfoNamespace . declInfo

data HtmlOutput a = HtmlOutput
  { htmlIndex     :: [(Maybe Char, a)]
  , htmlModules   :: [(P.ModuleName, HtmlOutputModule a)]
  }
  deriving (Show, Functor)

data HtmlOutputModule a = HtmlOutputModule
  { htmlOutputModuleLocals    :: a
  , htmlOutputModuleReExports :: [(InPackage P.ModuleName, a)]
  }
  deriving (Show, Functor)

data HtmlRenderContext = HtmlRenderContext
  { currentModuleName :: P.ModuleName
  , buildDocLink :: Namespace -> Text -> ContainingModule -> Maybe DocLink
  , renderDocLink :: DocLink -> Text
  , renderSourceLink :: P.SourceSpan -> Maybe Text
  }

-- |
-- An HtmlRenderContext for when you don't want to render any links.
nullRenderContext :: P.ModuleName -> HtmlRenderContext
nullRenderContext mn = HtmlRenderContext
  { currentModuleName = mn
  , buildDocLink = const (const (const Nothing))
  , renderDocLink = const ""
  , renderSourceLink = const Nothing
  }

packageAsHtml
    :: (InPackage P.ModuleName -> Maybe HtmlRenderContext)
    -> Package a
    -> HtmlOutput Html
packageAsHtml getHtmlCtx Package{..} =
  HtmlOutput indexFile modules
  where
  indexFile = []
  modules = moduleAsHtml getHtmlCtx <$> pkgModules

moduleAsHtml
    :: (InPackage P.ModuleName -> Maybe HtmlRenderContext)
    -> Module
    -> (P.ModuleName, HtmlOutputModule Html)
moduleAsHtml getR Module{..} = (modName, HtmlOutputModule modHtml reexports)
  where
  modHtml = do
    let r = fromMaybe (nullRenderContext modName) $ getR (Local modName)
     in do
        for_ modComments renderMarkdown
        for_ modDeclarations (declAsHtml r)
  reexports =
    flip map modReExports $ \(pkg, decls) ->
        let r = fromMaybe (nullRenderContext modName) $ getR pkg
         in (pkg, foldMap (declAsHtml r) decls)

-- renderIndex :: LinksContext -> [(Maybe Char, Html)]
-- renderIndex LinksContext{..} = go ctxBookmarks
--   where
--   go = takeLocals
--      >>> groupIndex getIndex renderEntry
--      >>> map (second (ul . mconcat))
-- 
--   getIndex (_, title_) = do
--     c <- textHeadMay title_
--     guard (toUpper c `elem` ['A'..'Z'])
--     pure c
-- 
--   textHeadMay t =
--     case T.length t of
--       0 -> Nothing
--       _ -> Just (T.index t 0)
-- 
--   renderEntry (mn, title_) =
--     li $ do
--       let url = T.pack (filePathFor mn `relativeTo` "index") <> "#" <> title_
--       code $
--         a ! A.href (v url) $ text title_
--       sp
--       text ("(" <> P.runModuleName mn <> ")")
-- 
--   groupIndex :: Ord i => (a -> Maybe i) -> (a -> b) -> [a] -> [(Maybe i, [b])]
--   groupIndex f g =
--     map (second DList.toList) . M.toList . foldr go' M.empty . sortBy (comparing f)
--     where
--     go' x = insertOrAppend (f x) (g x)
--     insertOrAppend idx val m =
--       let cur = M.findWithDefault DList.empty idx m
--           new = DList.snoc cur val
--       in  M.insert idx new m

declAsHtml :: HtmlRenderContext -> Declaration -> Html
declAsHtml r d@Declaration{..} = do
  let declFragment = makeFragment (declInfoNamespace declInfo) declTitle
  H.div ! A.class_ "decl" ! A.id (v (T.drop 1 declFragment)) $ do
    h3 ! A.class_ "decl__title clearfix" $ do
      a ! A.class_ "decl__anchor" ! A.href (v declFragment) $ "#"
      H.span $ text declTitle
      text " " -- prevent browser from treating
               -- declTitle + linkToSource as one word
      for_ declSourceSpan (linkToSource r)

    H.div ! A.class_ "decl__body" $ do
      case declInfo of
        AliasDeclaration fixity alias_ ->
          renderAlias fixity alias_
        _ ->
          pre ! A.class_ "decl__signature" $ code $
            codeAsHtml r (Render.renderDeclaration d)

      for_ declComments renderMarkdown

      let (instances, dctors, members) = partitionChildren declChildren

      unless (null dctors) $ do
        h4 "Constructors"
        renderChildren r dctors

      unless (null members) $ do
        h4 "Members"
        renderChildren r members

      unless (null instances) $ do
        h4 "Instances"
        renderChildren r instances
  where
    linkToSource :: HtmlRenderContext -> P.SourceSpan -> Html
    linkToSource ctx srcspan =
      maybe (return ()) go (renderSourceLink ctx srcspan)
      where
      go href =
        H.span ! A.class_ "decl__source" $
          a ! A.href (v href) $ text "Source"

renderChildren :: HtmlRenderContext -> [ChildDeclaration] -> Html
renderChildren _ [] = return ()
renderChildren r xs = ul $ mapM_ item xs
  where
  item decl =
    li ! A.id (v (T.drop 1 (fragment decl))) $ do
      renderCode decl
      for_ (cdeclComments decl) $ \coms ->
        H.div ! A.class_ "decl__child_comments" $ renderMarkdown coms

  fragment decl = makeFragment (childDeclInfoNamespace (cdeclInfo decl)) (cdeclTitle decl)
  renderCode = code . codeAsHtml r . Render.renderChildDeclaration

codeAsHtml :: HtmlRenderContext -> RenderedCode -> Html
codeAsHtml r = outputWith elemAsHtml
  where
  elemAsHtml e = case e of
    Syntax x ->
      withClass "syntax" (text x)
    Keyword x ->
      withClass "keyword" (text x)
    Space ->
      text " "
    Symbol ns name link_ ->
      case link_ of
        Link mn ->
          let
            class_ =
              if startsWithUpper name then "ctor" else "ident"
            target
              | isOp name =
                  if ns == TypeLevel
                    then "type (" <> name <> ")"
                    else "(" <> name <> ")"
              | otherwise = name
          in
            linkToDecl ns target mn (withClass class_ (text name))
        NoLink ->
          text name

  linkToDecl = linkToDeclaration r

  startsWithUpper :: Text -> Bool
  startsWithUpper str =
    if T.null str
      then False
      else isUpper (T.index str 0)

  isOp = isRight . runParser P.symbol

  runParser :: P.TokenParser a -> Text -> Either String a
  runParser p' s = either (Left . show) Right $ do
    ts <- P.lex "" s
    P.runTokenParser "" (p' <* eof) ts

renderLink :: HtmlRenderContext -> DocLink -> Html -> Html
renderLink r link_@DocLink{..} =
  a ! A.href (v (renderDocLink r link_ <> fragmentFor link_))
    ! A.title (v fullyQualifiedName)
  where
  fullyQualifiedName = case linkLocation of
    SameModule                -> fq (currentModuleName r) linkTitle
    LocalModule _ modName     -> fq modName linkTitle
    DepsModule _ _ _ modName  -> fq modName linkTitle
    BuiltinModule modName     -> fq modName linkTitle

  fq mn str = P.runModuleName mn <> "." <> str

makeFragment :: Namespace -> Text -> Text
makeFragment ns = (prefix <>) . escape
  where
  prefix = case ns of
    TypeLevel -> "#t:"
    ValueLevel -> "#v:"
    KindLevel -> "#k:"

  -- TODO
  escape = id

fragmentFor :: DocLink -> Text
fragmentFor l = makeFragment (linkNamespace l) (linkTitle l)

linkToDeclaration ::
  HtmlRenderContext ->
  Namespace ->
  Text ->
  ContainingModule ->
  Html ->
  Html
linkToDeclaration r ns target containMn =
  maybe id (renderLink r) (buildDocLink r ns target containMn)

renderAlias :: P.Fixity -> FixityAlias -> Html
renderAlias (P.Fixity associativity precedence) alias_ =
  p $ do
    -- TODO: Render a link
    toHtml $ "Operator alias for " <> P.showQualified showAliasName alias_ <> " "
    em $
      text ("(" <> associativityStr <> " / precedence " <> T.pack (show precedence) <> ")")
  where
  showAliasName (Left valueAlias) = P.runProperName valueAlias
  showAliasName (Right typeAlias) = case typeAlias of
    (Left identifier)  -> P.runIdent identifier
    (Right properName) -> P.runProperName properName
  associativityStr = case associativity of
    P.Infixl -> "left-associative"
    P.Infixr -> "right-associative"
    P.Infix  -> "non-associative"

-- | Render Markdown to HTML. Safe for untrusted input. Relative links are
-- | removed.
renderMarkdown :: Text -> H.Html
renderMarkdown =
  H.toMarkup . removeRelativeLinks . Cheapskate.markdown opts
  where
  opts = Cheapskate.def { Cheapskate.allowRawHtml = False }

removeRelativeLinks :: Cheapskate.Doc -> Cheapskate.Doc
removeRelativeLinks = Cheapskate.walk go
  where
  go :: Cheapskate.Inlines -> Cheapskate.Inlines
  go = (>>= stripRelatives)

  stripRelatives :: Cheapskate.Inline -> Cheapskate.Inlines
  stripRelatives (Cheapskate.Link contents_ href _)
    | isRelativeURI href = contents_
  stripRelatives other = pure other

  -- Tests for a ':' character in the first segment of a URI.
  --
  -- See Section 4.2 of RFC 3986:
  -- https://tools.ietf.org/html/rfc3986#section-4.2
  --
  -- >>> isRelativeURI "http://example.com/" == False
  -- >>> isRelativeURI "mailto:me@example.com" == False
  -- >>> isRelativeURI "foo/bar" == True
  -- >>> isRelativeURI "/bar" == True
  -- >>> isRelativeURI "./bar" == True
  isRelativeURI :: Text -> Bool
  isRelativeURI =
    T.takeWhile (/= '/') >>> T.all (/= ':')

v :: Text -> AttributeValue
v = toValue

withClass :: String -> Html -> Html
withClass className content = H.span ! A.class_ (fromString className) $ content

partitionChildren ::
  [ChildDeclaration] ->
  ([ChildDeclaration], [ChildDeclaration], [ChildDeclaration])
partitionChildren =
  reverseAll . foldl go ([], [], [])
  where
  go (instances, dctors, members) rcd =
    case cdeclInfo rcd of
      ChildInstance _ _      -> (rcd : instances, dctors, members)
      ChildDataConstructor _ -> (instances, rcd : dctors, members)
      ChildTypeClassMember _ -> (instances, dctors, rcd : members)

  reverseAll (xs, ys, zs) = (reverse xs, reverse ys, reverse zs)
