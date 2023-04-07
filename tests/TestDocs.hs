module TestDocs where

import Prelude

import Data.Bifunctor (first)
import Data.List (findIndex)
import Data.Foldable (find, forM_)
import Safe (headMay)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid (Any(..), First(..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.PrettyPrint.Boxes qualified as Boxes

import Language.PureScript qualified as P
import Language.PureScript.Docs qualified as Docs
import Language.PureScript.Docs.AsMarkdown (codeToString)
import Language.PureScript.Publish.ErrorsWarnings qualified as Publish

import Web.Bower.PackageMeta (parsePackageName, runPackageName)

import TestPscPublish (preparePackage)

import Test.Hspec (Spec, beforeAll, context, expectationFailure, it)

spec :: Spec
spec = beforeAll (handleDocPrepFailure <$> preparePackage "tests/purs/docs" "purs.json" "resolutions.json") $
  context "Language.PureScript.Docs" $ do
    context "Doc generation tests:" $
      mkSpec testCases displayAssertion $ \a pkg mdl ->
        case runAssertion a (Docs.getLinksContext pkg) mdl of
          Pass ->
            pure ()
          Fail reason ->
            expectationFailure (T.unpack (displayAssertionFailure reason))

    context "Tag generation tests:" $
      mkSpec testTagsCases displayTagsAssertion $ \a _ mdl ->
        case runTagsAssertion a (Map.fromList $ Docs.tags mdl) of
          TagsPass ->
            pure ()
          TagsFail reason ->
            expectationFailure (T.unpack (displayTagsAssertionFailure reason))
  where
  handleDocPrepFailure = first (expectationFailure . ("failed to produce docs: " <>) . Boxes.render . Publish.renderError)

  mkSpec cases displayAssertion' runner =
    forM_ cases $ \(mnString, assertions) -> do
      let mn = P.moduleNameFromString mnString
      context ("in module " ++ T.unpack mnString) $
        forM_ assertions $ \a ->
          it (T.unpack (displayAssertion' a)) . either id $ \pkg@Docs.Package{..} ->
            case find ((==) mn . Docs.modName) pkgModules of
              Nothing ->
                expectationFailure ("module not found in docs: " ++ T.unpack mnString)
              Just mdl ->
                runner a pkg mdl

data DocsAssertion
  -- | Assert that a particular declaration is documented with the given
  -- children
  = ShouldBeDocumented P.ModuleName Text [Text]
  -- | Assert that a particular declaration is not documented
  | ShouldNotBeDocumented P.ModuleName Text
  -- | Assert that a particular declaration exists, but without a particular
  -- child.
  | ChildShouldNotBeDocumented P.ModuleName Text Text
  -- | Assert that a particular declaration has a particular type class
  -- constraint.
  | ShouldBeConstrained P.ModuleName Text Text
  -- | Assert that a particular typeclass declaration has a functional
  -- dependency list.
  | ShouldHaveFunDeps P.ModuleName Text [([Text],[Text])]
  -- | Assert that a particular value declaration exists, and its type
  -- satisfies the given predicate.
  | ValueShouldHaveTypeSignature P.ModuleName Text (Docs.Type' -> Bool)
  -- | Assert that a particular instance declaration exists under some class or
  -- type declaration, and that its type satisfies the given predicate.
  | InstanceShouldHaveTypeSignature P.ModuleName Text Text (Docs.Type' -> Bool)
  -- | Assert that a particular type alias exists, and its corresponding
  -- type, when rendered, matches a given string exactly
  -- fields: module, type synonym name, expected type
  | TypeSynonymShouldRenderAs P.ModuleName Text Text
  -- | Assert that a documented declaration includes a documentation comment
  -- containing a particular string
  | ShouldHaveDocComment P.ModuleName Text Text
  -- | Assert that a documented data declaration includes a documentation comment
  -- | containing a particular string
  | ShouldHaveDataConstructorDocComment P.ModuleName Text Text Text
  -- | Assert that a documented data declaration has no documentation comment
  | ShouldHaveNoDataConstructorDocComment P.ModuleName Text Text
  -- | Assert that a documented class method includes a documentation comment
  -- | containing a particular string
  | ShouldHaveClassMethodDocComment P.ModuleName Text Text Text
  -- | Assert that a class method has no documentation comment
  | ShouldNotHaveClassMethodDocComment P.ModuleName Text Text
  -- | Assert that there should be some declarations re-exported from a
  -- particular module in a particular package.
  | ShouldHaveReExport (Docs.InPackage P.ModuleName)
  -- | Assert that a link to some specific declaration exists within the
  -- rendered code for a declaration. Fields are: local module, local
  -- declaration title, title of linked declaration, namespace of linked
  -- declaration, destination of link.
  | ShouldHaveLink P.ModuleName Text Text Docs.Namespace Docs.LinkLocation
  -- | Assert that a given declaration comes before another in the output
  | ShouldComeBefore P.ModuleName Text Text
  -- | Assert that a given declaration has the given kind signature
  | ShouldHaveKindSignature P.ModuleName Text Text
  -- | Assert that a given declaration does not have a kind signature
  | ShouldNotHaveKindSignature P.ModuleName Text
  -- | Assert that a given declaration with doc-comments on its
  -- kind signature, type declaration, and role declaration are properly
  -- merged into one doc-comment.
  | ShouldMergeDocComments P.ModuleName Text (Maybe Text)
  -- | Assert that a given declaration's type parameters have the
  -- given role annotations
  | ShouldHaveRoleAnnotation P.ModuleName Text [P.Role]
  -- | Assert that a given module has the expected doc comments
  | ShouldHaveModuleDocs P.ModuleName (Maybe Text)

data TagsAssertion
  -- | Assert that a particular declaration is tagged
  = ShouldBeTagged Text Int
  -- | Assert that a particular declaration is not tagged
  | ShouldNotBeTagged Text

displayAssertion :: DocsAssertion -> Text
displayAssertion = \case
  ShouldBeDocumented mn decl children ->
    showQual mn decl <> " should be documented" <>
    (if not (null children)
       then " with children: " <> T.pack (show children)
       else "")
  ShouldNotBeDocumented mn decl ->
    showQual mn decl <> " should not be documented"
  ChildShouldNotBeDocumented mn decl child ->
    showQual mn decl <> " should not have " <> child <> " as a child declaration"
  ShouldBeConstrained mn decl constraint ->
    showQual mn decl <> " should have a " <> constraint <> " constraint"
  ShouldHaveFunDeps mn decl fundeps ->
    showQual mn decl <> " should have fundeps: " <> T.pack (show fundeps)
  ValueShouldHaveTypeSignature mn decl _ ->
    "the type signature for " <> showQual mn decl <>
    " should satisfy the given predicate"
  InstanceShouldHaveTypeSignature _ parent instName _ ->
    "the instance " <> instName <> " (under " <> parent <> ") should have" <>
    " a type signature satisfying the given predicate"
  TypeSynonymShouldRenderAs mn synName code ->
    "the RHS of the type synonym " <> showQual mn synName <>
    " should be rendered as " <> code
  ShouldHaveDocComment mn decl excerpt ->
    "the string " <> T.pack (show excerpt) <> " should appear in the" <>
    " doc-comments for " <> showQual mn decl
  ShouldHaveDataConstructorDocComment mn decl constr excerpt ->
    "the string " <> T.pack (show excerpt) <> " should appear in the" <>
    " doc-comments for data constructor " <> T.pack (show constr) <> " for " <> showQual mn decl
  ShouldHaveNoDataConstructorDocComment mn decl constr ->
    "Doc-comments for data constructor " <> T.pack (show constr) <> " for " <> showQual mn decl <>
    " should be empty"
  ShouldHaveClassMethodDocComment mn decl method excerpt ->
    "the string " <> T.pack (show excerpt) <> " should appear in the" <>
    " doc-comment for class method " <> T.pack (show method) <> " for " <> showQual mn decl
  ShouldNotHaveClassMethodDocComment mn decl method ->
    "Doc-comments for class method " <> T.pack (show method) <> " for " <> showQual mn decl <>
    " should be empty"
  ShouldHaveReExport inPkg ->
    "there should be some re-exports from " <>
    showInPkg P.runModuleName inPkg
  ShouldHaveLink mn decl targetTitle targetNs _ ->
    "the rendered code for " <> showQual mn decl <> " should contain a link" <>
    " to " <> targetTitle <> " (" <> T.pack (show targetNs) <> ")"
  ShouldComeBefore mn declA declB ->
    showQual mn declA <> " should come before " <> showQual mn declB <>
    " in the docs"
  ShouldHaveKindSignature mn decl expected ->
    showQual mn decl <> " should have the kind signature `" <> expected <> "`"
  ShouldNotHaveKindSignature mn decl ->
    showQual mn decl <> " should not have a kind signature."
  ShouldMergeDocComments mn decl _ ->
    showQual mn decl <> " should merge the doc-comments of its kind " <>
    "declaration (if any), type declaration, and role declaration (if any) " <>
    "into one doc-comment."
  ShouldHaveRoleAnnotation mn decl expected ->
    showQual mn decl <> " should have the expected role annotations: " <>
    T.intercalate ", " (fmap P.displayRole expected)
  ShouldHaveModuleDocs mn expected ->
    "Module doc comments for module `" <> P.runModuleName mn <> "` should be " <>
      maybe "empty" (\t -> "'" <> t <> "`") expected

displayTagsAssertion :: TagsAssertion -> Text
displayTagsAssertion = \case
  ShouldBeTagged decl l ->
    decl <> " should be tagged at line " <> T.pack (show l)
  ShouldNotBeTagged decl ->
    decl <> " should not be tagged"

data DocsAssertionFailure
  -- | A declaration was not documented, but should have been
  = NotDocumented P.ModuleName Text
  -- | The expected list of child declarations did not match the actual list
  | ChildrenNotDocumented P.ModuleName Text [Text]
  -- | A declaration was documented, but should not have been
  | Documented P.ModuleName Text
  -- | A child declaration was documented, but should not have been
  | ChildDocumented P.ModuleName Text Text
  -- | A constraint was missing.
  | ConstraintMissing P.ModuleName Text Text
  -- | A functional dependency was missing.
  | FunDepMissing P.ModuleName Text [([Text], [Text])]
  -- | A declaration had the wrong "type" (ie, value, type, type class)
  -- Fields: declaration title, expected "type", actual "type".
  | WrongDeclarationType P.ModuleName Text Text Text
  -- | A declaration had the wrong type (in the sense of "type checking"), eg,
  -- because the inferred type was used when the explicit type should have
  -- been.
  -- Fields: module name, declaration name, actual type.
  | DeclarationWrongType P.ModuleName Text Docs.Type'
  -- | A Type synonym has been rendered in an unexpected format
  -- Fields: module name, declaration name, expected rendering, actual rendering
  | TypeSynonymMismatch P.ModuleName Text Text Text
  -- | A doc comment was not found or did not match what was expected
  -- Fields: module name, declaration, actual comments
  | DocCommentMissing P.ModuleName Text (Maybe Text)
  -- | A doc comment was found where none was expected
  -- Fields: module name, declaration, actual comments
  | DocCommentPresent P.ModuleName Text (Maybe Text)
  -- | A module was missing re-exports from a particular module.
  -- Fields: module name, expected re-export, actual re-exports.
  | ReExportMissing P.ModuleName (Docs.InPackage P.ModuleName) [Docs.InPackage P.ModuleName]
  -- | Expected to find some other declaration mentioned in this declaration's
  -- rendered code, but did not find anything.
  -- Fields: module name, declaration title, title of declaration which was
  -- expected but not found in.
  | LinkedDeclarationMissing P.ModuleName Text Text
  -- | Expected one link location for a declaration mentioned in some other
  -- declaration's rendered code, but found a different one. Fields: module
  -- name, title of the local declaration which links to some other
  -- declaration, title of the linked declaration, expected location, actual
  -- location.
  | BadLinkLocation P.ModuleName Text Text Docs.LinkLocation Docs.LinkLocation
  -- | Declarations were in the wrong order
  | WrongOrder P.ModuleName Text Text
  -- | Expected a kind signature for a declaration, but did not find one
  -- Fields: module name, declaration title.
  | KindSignatureMissing P.ModuleName Text
  -- | The rendered kind signature did not match the expected one.
  -- Fields: module name, declaration title, expected kind signature (text),
  -- actual kind signature (text), actual kind signature (structure)
  | KindSignatureMismatch P.ModuleName Text Text Text (P.Type ())
  -- | A kind signature was found where none was expected.
  -- Fields: module name, declaration title, actual kind signature (text),
  -- actual kind signature (structure)
  | KindSignaturePresent P.ModuleName Text Text (P.Type ())
  -- | The doc comments for the kind signature (if any), type declaration, and
  -- role declaration (if any) were not properly merged into the expected one.
  -- Fields: module name, declaration title, expected doc-comments,
  -- actual doc-comments
  | DocCommentMergeFailure P.ModuleName Text Text Text
  -- | The given declaration cannot have role annotations.
  -- Fields: module name, declaration title
  | CannotHaveRoles P.ModuleName Text
  -- | The list of expected roles did not match the list of actual roles
  -- fields: module name, declaration title, expected role list,
  -- actual role list
  | RoleMismatch P.ModuleName Text [P.Role] [P.Role]
  -- | The module's doc comments should be the expected
  -- fields: module name, expected docs, actual docs
  | WrongModuleDocs P.ModuleName (Maybe Text) (Maybe Text)

data TagsAssertionFailure
  -- | A declaration was not tagged, but should have been
  = NotTagged Text
  -- | A declaration was tagged, but should not have been
  | Tagged Text Int
  -- | A declaration was tagged on the wrong line
  | TaggedWrongLine Text Int Int

displayAssertionFailure :: DocsAssertionFailure -> Text
displayAssertionFailure = \case
  NotDocumented _ decl ->
    decl <> " was not documented, but should have been"
  ChildrenNotDocumented _ decl children ->
    decl <> " had the wrong children; got " <> T.pack (show children)
  Documented _ decl ->
    decl <> " was documented, but should not have been"
  ChildDocumented _ decl child ->
    decl <> " had " <> child <> " as a child"
  ConstraintMissing _ decl constraint ->
    decl <> " did not have a " <> constraint <> " constraint"
  FunDepMissing _ decl fundeps ->
    decl <> " had the wrong fundeps; got " <> T.pack (show fundeps)
  WrongDeclarationType _ decl expected actual ->
    "expected " <> decl <> " to be a " <> expected <> " declaration, but it" <>
    " was a " <> actual <> " declaration"
  DeclarationWrongType _ decl actual ->
    decl <> " had the wrong type; got " <> T.pack (P.prettyPrintType maxBound actual)
  TypeSynonymMismatch _ decl expected actual ->
    "expected the RHS of " <> decl <> " to be " <> expected <>
    "; got " <> actual
  DocCommentMissing _ decl actual ->
    "the doc-comment for " <> decl <> " did not contain the expected substring;" <>
    " got " <> T.pack (show actual)
  DocCommentPresent _ decl actual ->
    "the doc-comment for " <> decl <> " was not empty. Got " <> T.pack (show actual)
  ReExportMissing _ expected actuals ->
    "expected to see some re-exports from " <>
    showInPkg P.runModuleName expected <>
    "; instead only saw re-exports from " <>
    T.pack (show (map (showInPkg P.runModuleName) actuals))
  LinkedDeclarationMissing _ decl target ->
    "expected to find a link to " <> target <> " within the rendered code" <>
    " for " <> decl <> ", but no such link was found"
  BadLinkLocation _ decl target expected actual ->
    "in rendered code for " <> decl <> ", bad link location for " <> target <>
    ": expected " <> T.pack (show expected) <>
    " got " <> T.pack (show actual)
  WrongOrder _ before' after' ->
    "expected to see " <> before' <> " before " <> after'
  KindSignatureMissing _ decl ->
    "the kind signature for " <> decl <> " is missing."
  KindSignatureMismatch _ decl expected actualTxt actualKind ->
    "expected the kind signature for " <> decl <> "\n" <>
    "to be `" <> expected <> "`\n" <>
    "  got `" <> actualTxt <> "`\n" <>
    "Structure of kind: " <> T.pack (show actualKind)
  KindSignaturePresent _ decl actualTxt actualKind ->
    "the kind signature for " <> decl <> " was not empty.\n" <>
    "got `" <> actualTxt <> "`\n" <>
    "Structure of kind: " <> T.pack (show actualKind)
  DocCommentMergeFailure _ decl expected actual ->
    "Expected the doc-comment for " <> decl <> " to merge comments and be `" <>
    expected <> "`; got `" <> actual <> "`"
  CannotHaveRoles _ decl ->
    decl <> " is a type of declaration that cannot have roles."
  RoleMismatch _ decl expected actual ->
    "Expected the role annotations for " <> decl <> " to be \n" <>
    "`" <> displayRoleList expected <> "`, but got\n" <>
    "`" <> displayRoleList actual <> "`"
    where
      displayRoleList = T.intercalate ", " . fmap P.displayRole
  WrongModuleDocs mn expected actual ->
    "Expected module docs for " <> P.runModuleName mn <> "\n" <>
    "to be `" <> fromMaybe "<Nothing>" expected <> "`\n" <>
    "  got `" <> fromMaybe "<Nothing>" actual <> "`"

displayTagsAssertionFailure :: TagsAssertionFailure -> Text
displayTagsAssertionFailure = \case
  NotTagged decl ->
    decl <> " was not tagged, but should have been"
  Tagged decl line ->
    decl <> " was tagged at line " <> T.pack (show line) <>
    ", but should not have been"
  TaggedWrongLine decl taggedLine desiredLine ->
    decl <> " was tagged at line " <> T.pack (show taggedLine) <>
    ", but should have been tagged at line " <> T.pack (show desiredLine)

data DocsAssertionResult
  = Pass
  | Fail DocsAssertionFailure

data TagsAssertionResult
  = TagsPass
  | TagsFail TagsAssertionFailure

runAssertion :: DocsAssertion -> Docs.LinksContext -> Docs.Module -> DocsAssertionResult
runAssertion assertion linksCtx Docs.Module{..} =
  case assertion of
    ShouldBeDocumented mn decl children ->
      case findChildren decl (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just actualChildren ->
          if children == actualChildren
            then Pass
            else Fail (ChildrenNotDocumented mn decl actualChildren)

    ShouldNotBeDocumented mn decl ->
      case findChildren decl (declarationsFor mn) of
        Just _ ->
          Fail (Documented mn decl)
        Nothing ->
          Pass

    ChildShouldNotBeDocumented mn decl child ->
      case findChildren decl (declarationsFor mn) of
        Just children ->
          if child `elem` children
            then Fail (ChildDocumented mn decl child)
            else Pass
        Nothing ->
          Fail (NotDocumented mn decl)

    ShouldBeConstrained mn decl tyClass ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        case declInfo of
          Docs.ValueDeclaration ty ->
            if checkConstrained ty tyClass
              then Pass
              else Fail (ConstraintMissing mn decl tyClass)
          _ ->
            Fail (WrongDeclarationType mn decl "value"
                   (Docs.declInfoToString declInfo))

    ShouldHaveFunDeps mn decl fds ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        case declInfo of
          Docs.TypeClassDeclaration _ _ fundeps ->
            if fundeps == fds
              then Pass
              else Fail (FunDepMissing mn decl fds)
          _ ->
            Fail (WrongDeclarationType mn decl "value"
                   (Docs.declInfoToString declInfo))

    ValueShouldHaveTypeSignature mn decl tyPredicate ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        case declInfo of
          Docs.ValueDeclaration ty ->
            if tyPredicate ty
              then Pass
              else Fail (DeclarationWrongType mn decl ty)
          _ ->
            Fail (WrongDeclarationType mn decl "value"
                   (Docs.declInfoToString declInfo))

    InstanceShouldHaveTypeSignature mn parent decl tyPredicate ->
      case find ((==) parent . Docs.declTitle) (declarationsFor mn) >>= findTarget of
        Just ty ->
          if tyPredicate ty
            then Pass
            else Fail (DeclarationWrongType mn decl ty)
        Nothing ->
          Fail (NotDocumented mn decl)

      where
      findTarget =
        headMay .
        mapMaybe (extractInstanceType . Docs.cdeclInfo) .
        filter (\cdecl -> Docs.cdeclTitle cdecl == decl) .
        Docs.declChildren

      extractInstanceType = \case
        (Docs.ChildInstance _ ty) ->
          Just ty
        _ ->
          Nothing

    TypeSynonymShouldRenderAs mn decl expected ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        case declInfo of
          Docs.TypeSynonymDeclaration [] ty ->
            let actual = codeToString (Docs.renderType ty) in
            if actual == expected
               then Pass
               else Fail (TypeSynonymMismatch mn decl expected actual)
          _ ->
            Fail (WrongDeclarationType mn decl "synonym"
                  (Docs.declInfoToString declInfo))

    ShouldHaveDocComment mn decl expected ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        if maybe False (expected `T.isInfixOf`) declComments
          then Pass
          else Fail (DocCommentMissing mn decl declComments)

    ShouldHaveDataConstructorDocComment mn decl constr expected ->
      findDeclChildrenComment mn decl constr expected

    ShouldHaveNoDataConstructorDocComment mn decl constr ->
      findDeclChildrenNoComment mn decl constr

    ShouldHaveClassMethodDocComment mn decl constr expected ->
      findDeclChildrenComment mn decl constr expected

    ShouldNotHaveClassMethodDocComment mn decl method ->
      findDeclChildrenNoComment mn decl method

    ShouldHaveReExport reExp ->
      let
        reExps = map fst modReExports
      in
        if reExp `elem` reExps
          then Pass
          else Fail (ReExportMissing modName reExp reExps)

    ShouldHaveLink mn decl destTitle destNs expectedLoc ->
      findDecl mn decl $ \decl' ->
        let
          rendered = Docs.renderDeclaration decl'
        in
          case extract rendered destNs destTitle of
            Just (Docs.linkLocation -> actualLoc) ->
              if expectedLoc == actualLoc
                then Pass
                else Fail (BadLinkLocation mn decl destTitle expectedLoc actualLoc)
            Nothing ->
              Fail (LinkedDeclarationMissing mn decl destTitle)

    ShouldComeBefore mn before' after' ->
      let
        decls = declarationsFor mn

        indexOf :: Text -> Maybe Int
        indexOf title = findIndex ((==) title . Docs.declTitle) decls
      in
        case (indexOf before', indexOf after') of
          (Just i, Just j) ->
            if i < j
              then Pass
              else Fail (WrongOrder mn before' after')
          (Nothing, _) ->
            Fail (NotDocumented mn before')
          (_, Nothing) ->
            Fail (NotDocumented mn after')

    ShouldHaveKindSignature mn decl expected ->
      findDeclKinds mn decl $ \case
        Just Docs.KindInfo{..} ->
          if expected /= actual
              then Fail (KindSignatureMismatch mn decl expected actual kiKind)
              else Pass
          where
            actual = codeToString $ Docs.renderKindSig decl $
              Docs.KindInfo kiKeyword kiKind
        Nothing -> Fail (KindSignatureMissing mn decl)

    ShouldNotHaveKindSignature mn decl ->
      findDeclKinds mn decl $ \case
        Just Docs.KindInfo{..} ->
          Fail (KindSignaturePresent mn decl actual kiKind)
          where
            actual = codeToString $ Docs.renderKindSig decl $
              Docs.KindInfo kiKeyword kiKind
        Nothing -> Pass

    ShouldMergeDocComments mn decl expected ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        if expected == declComments
            then Pass
            else Fail (DocCommentMergeFailure mn decl (display expected) (display declComments))
        where
          display = fromMaybe ""

    ShouldHaveRoleAnnotation mn decl expected ->
      findDeclRoles mn decl $ \actual ->
        if expected == actual
          then Pass
          else Fail (RoleMismatch mn decl expected actual)

    ShouldHaveModuleDocs mn expected ->
      if expected == modComments then
        Pass
      else
        Fail (WrongModuleDocs mn expected modComments)
  where
  declarationsFor mn =
    if mn == modName
      then modDeclarations
      else fromMaybe [] (lookup mn (map (first Docs.ignorePackage) modReExports))

  findChildren title =
    fmap childrenTitles . find ((==) title . Docs.declTitle)

  findDecl mn title f =
      case find ((==) title . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn title)
        Just decl ->
          f decl

  findDeclKinds mn title f =
      case find ((==) title . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn title)
        Just Docs.Declaration{..} ->
          f declKind

  findDeclRoles mn title f =
      case find ((==) title . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn title)
        Just Docs.Declaration{..} ->
          case getRoles declInfo of
            Nothing ->
              Fail (CannotHaveRoles mn title)
            Just roles ->
              f roles

  findDeclChildren mn title child f =
    findDecl mn title $ \Docs.Declaration{..} ->
      case find ((==) child . Docs.cdeclTitle) declChildren of
        Nothing ->
          Fail (NotDocumented mn child)
        Just decl ->
          f decl

  findDeclChildrenComment mn decl constr expected =
    findDeclChildren mn decl constr $ \Docs.ChildDeclaration{..} ->
      if maybe False (expected `T.isInfixOf`) cdeclComments
        then Pass
        else Fail (DocCommentMissing mn constr cdeclComments)

  findDeclChildrenNoComment mn decl constr =
    findDeclChildren mn decl constr $ \Docs.ChildDeclaration{..} ->
      if isNothing cdeclComments
      then Pass
      else Fail (DocCommentPresent mn constr cdeclComments)

  childrenTitles = map Docs.cdeclTitle . Docs.declChildren

  getRoles :: Docs.DeclarationInfo -> Maybe [P.Role]
  getRoles = \case
    Docs.DataDeclaration _ _ roles -> Just roles
    _ -> Nothing

  extract :: Docs.RenderedCode -> Docs.Namespace -> Text -> Maybe Docs.DocLink
  extract rc ns title = getFirst (Docs.outputWith (First . go) rc) >>= getLink
    where
      getLink =
        Docs.getLink linksCtx (P.moduleNameFromString "$DocsTest") ns title
      go = \case
        Docs.Symbol ns' title' (Docs.Link containingMod)
          | ns' == ns && title' == title -> Just containingMod
        _ ->
          Nothing

runTagsAssertion :: TagsAssertion -> Map.Map String Int -> TagsAssertionResult
runTagsAssertion assertion tags =
  case assertion of
    ShouldBeTagged decl line ->
      case Map.lookup (T.unpack decl) tags of
        Just taggedLine ->
          if taggedLine == line
            then TagsPass
            else TagsFail $ TaggedWrongLine decl taggedLine line
        Nothing -> TagsFail $ NotTagged decl

    ShouldNotBeTagged decl ->
      case Map.lookup (T.unpack decl) tags of
        Just taggedLine -> TagsFail $ Tagged decl taggedLine
        Nothing -> TagsPass

checkConstrained :: P.Type a -> Text -> Bool
checkConstrained ty tyClass =
  case ty of
    P.ConstrainedType _ c ty'
      | matches tyClass c -> True
      | otherwise -> checkConstrained ty' tyClass
    P.ForAll _ _ _ _ ty' _ ->
      checkConstrained ty' tyClass
    _ ->
      False
  where
  matches className =
    (==) className . P.runProperName . P.disqualify . P.constraintClass

testCases :: [(Text, [DocsAssertion])]
testCases =
  [ ("Example",
      [ -- From dependencies
        ShouldBeDocumented    (n "Prelude") "Unit" []
      , ShouldNotBeDocumented (n "Prelude") "unit"

        -- From local files
      , ShouldBeDocumented    (n "Example2") "one" []
      , ShouldNotBeDocumented (n "Example2") "two"

        -- Re-exports
      , ShouldHaveReExport (Docs.FromDep (pkg "purescript-prelude") (n "Prelude"))
      , ShouldHaveReExport (Docs.Local (n "Example2"))
      ])

  , ("Example2",
      [ ShouldBeDocumented (n "Example2") "one" []
      , ShouldBeDocumented (n "Example2") "two" []

      , ShouldHaveLink (n "Example2") "one" "Int" Docs.TypeLevel (Docs.BuiltinModule (n "Prim"))
      ])

  , ("UTF8",
      [ ShouldBeDocumented (n "UTF8") "thing" []
      ])

  , ("Transitive1",
      [ ShouldBeDocumented (n "Transitive2") "transitive3" []
      ])

  , ("NotAllCtors",
      [ ShouldBeDocumented         (n "Prelude") "Boolean2" ["True"]
      , ChildShouldNotBeDocumented (n "Prelude") "Boolean2" "False"
      ])

  , ("DuplicateNames",
      [ ShouldBeDocumented    (n "Prelude")        "Unit" []
      , ShouldBeDocumented    (n "DuplicateNames") "unit" []
      , ShouldNotBeDocumented (n "Prelude")        "unit"
      ])

  , ("MultiVirtual",
      [ ShouldBeDocumented (n "MultiVirtual1") "foo" []
      , ShouldBeDocumented (n "MultiVirtual2") "bar" []
      , ShouldBeDocumented (n "MultiVirtual2") "baz" []
      ])

  , ("Clash",
      [ ShouldBeDocumented (n "Clash1") "value" []
      , ShouldBeDocumented (n "Clash1") "Type'" []
      , ShouldBeDocumented (n "Clash1") "TypeClass" ["typeClassMember"]
      ])

  , ("SolitaryTypeClassMember",
      [ ShouldBeDocumented    (n "SomeTypeClass") "member" []
      , ShouldNotBeDocumented (n "SomeTypeClass") "SomeClass"
      , ShouldBeConstrained   (n "SomeTypeClass") "member" "SomeClass"
      ])

  , ("ReExportedTypeClass",
      [ ShouldBeDocumented (n "SomeTypeClass") "SomeClass" ["member"]
      ])

  , ("TypeClassWithoutMembers",
      [ ShouldBeDocumented         (n "TypeClassWithoutMembersIntermediate") "SomeClass" []
      , ChildShouldNotBeDocumented (n "TypeClassWithoutMembersIntermediate") "SomeClass" "member"
      ])

  , ("TypeClassWithFunDeps",
      [ ShouldHaveFunDeps          (n "TypeClassWithFunDeps") "TypeClassWithFunDeps" [(["a","b"], ["c"]), (["c"], ["d","e"])]
      ])

  , ("NewOperators",
      [ ShouldBeDocumented (n "NewOperators2") "(>>>)" []
      ])

  , ("ExplicitTypeSignatures",
      [ ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "explicit" (hasTypeVar "something")
      , ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "anInt"    (P.tyInt `P.eqType`)
      , ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "aNumber"  (P.tyNumber `P.eqType`)
      ])

  , ("ConstrainedArgument",
      [ TypeSynonymShouldRenderAs (n "ConstrainedArgument") "WithoutArgs" "forall a. (Partial => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "WithArgs" "forall a. (Foo a => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "MultiWithoutArgs" "forall a. (Partial => Partial => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "MultiWithArgs" "forall a b. (Foo a => Foo b => a) -> a"
      ])

  , ("TypeOpAliases",
      [ ValueShouldHaveTypeSignature (n "TypeOpAliases") "test1" (renderedType "forall a b. a ~> b")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test2" (renderedType "forall a b c. a ~> b ~> c")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test3" (renderedType "forall a b c d. a ~> (b ~> c) ~> d")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test4" (renderedType "forall a b c d. ((a ~> b) ~> c) ~> d")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "third" (renderedType "forall a b c. a × b × c -> c")

      , ShouldBeDocumented (n "TypeOpAliases") "Tuple" ["Tuple","showTuple", "testLEither", "testREither"]
      , ShouldBeDocumented (n "TypeOpAliases") "Either" ["Left", "Right","testLEither", "testREither"]
      , ShouldBeDocumented (n "TypeOpAliases") "Show" ["show","showTuple"]

      , InstanceShouldHaveTypeSignature (n "TypeOpAliases") "Either" "testLEither" (renderedType "TestL (Either Int (Tuple Int String))")
      , InstanceShouldHaveTypeSignature (n "TypeOpAliases") "Either" "testREither" (renderedType "TestR (Either (Tuple Int Int) String)")
      ])

  , ("DocComments",
      [ ShouldHaveDocComment (n "DocComments") "example" "    example == 0"
      ])

  , ("DocCommentsDataConstructor",
      [ ShouldHaveDataConstructorDocComment (n "DocCommentsDataConstructor") "Foo" "Bar" "data constructor comment"
      , ShouldHaveNoDataConstructorDocComment (n "DocCommentsDataConstructor") "Foo" "Baz"
      , ShouldHaveNoDataConstructorDocComment (n "DocCommentsDataConstructor") "ComplexFoo" "ComplexBar"
      , ShouldHaveDataConstructorDocComment (n "DocCommentsDataConstructor") "ComplexFoo" "ComplexBaz" "another data constructor comment"
      , ShouldHaveDataConstructorDocComment (n "DocCommentsDataConstructor") "NewtypeFoo" "NewtypeFoo" "newtype data constructor comment"
      ])

  , ("DocCommentsClassMethod",
      [ ShouldHaveClassMethodDocComment (n "DocCommentsClassMethod") "Foo" "bar" "class method comment"
      , ShouldNotHaveClassMethodDocComment (n "DocCommentsClassMethod") "Foo" "baz"
      ])

  , ("TypeLevelString",
      [ ShouldBeDocumented (n "TypeLevelString") "Foo" ["fooBar"]
      ])

  , ("Desugar",
      [ ValueShouldHaveTypeSignature (n "Desugar") "test" (renderedType "forall a b. X (a -> b) a -> b")
      ])

  , ("ChildDeclOrder",
      [ ShouldBeDocumented (n "ChildDeclOrder") "Two" ["First", "Second", "showTwo", "fooTwo"]
      , ShouldBeDocumented (n "ChildDeclOrder") "Foo" ["foo1", "foo2", "fooTwo", "fooInt"]
      ])

  , ("DeclOrder",
      shouldBeOrdered (n "DeclOrder")
        ["A", "x1", "X2", "x3", "X4", "B"])

  , ("DeclOrderNoExportList",
      shouldBeOrdered (n "DeclOrderNoExportList")
        [ "x1", "x3", "X2", "X4", "A", "B" ])

  , ("Ado",
      [ ValueShouldHaveTypeSignature (n "Ado") "test" (renderedType "Int")
      ]
    )

  , ("TypeSynonymInstance",
      [ ShouldBeDocumented (n "TypeSynonymInstance") "MyNT" ["MyNT", "ntMyNT"]
      ]
    )
  , ("KindSignatureDocs",
      -- expected kind signatures
      [ ShouldHaveKindSignature (n "KindSignatureDocs") "DKindAndType" "data DKindAndType :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "TKindAndType" "type TKindAndType :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "NKindAndType" "newtype NKindAndType :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "CKindAndType" "class CKindAndType :: forall k. (k -> Type) -> k -> Constraint"

      , ShouldHaveKindSignature (n "KindSignatureDocs") "DKindOnly" "data DKindOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "TKindOnly" "type TKindOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "NKindOnly" "newtype NKindOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "CKindOnly" "class CKindOnly :: forall k. (k -> Type) -> k -> Constraint"

      , ShouldHaveKindSignature (n "KindSignatureDocs") "DTypeOnly" "data DTypeOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "TTypeOnly" "type TTypeOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "NTypeOnly" "newtype NTypeOnly :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "CTypeOnly" "class CTypeOnly :: forall k. (k -> Type) -> k -> Constraint"

      -- Declarations with no explicit kind signatures should still have
      -- their inferred kind signatures displayed as long as at least one
      -- type parameter does not have kind `Type`.
      , ShouldHaveKindSignature (n "KindSignatureDocs") "DImplicit" "data DImplicit :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "TImplicit" "type TImplicit :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "NImplicit" "newtype NImplicit :: forall k. k -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "CImplicit" "class CImplicit :: forall k1. (k1 -> Type) -> k1 -> Constraint"

      -- Declarations with no explicit kind signatures should not be displayed
      -- if each type parameter in their inferred kind signature
      -- has kind `Type`.
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DHidden"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DNothing"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "THidden"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "NHidden"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "CHidden"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "CNothing"

      -- FFI declarations always have an explicit kind signature
      -- but only show them if they are "interesting."
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "FFI_Hidden"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "FFI_Shown" "data FFI_Shown :: (Type -> Type) -> Type"

      -- Declarations with an explicit kind signature that is wrapped
      -- in parenthesis at various points, but which "desugars" so to speak
      -- to an uninteresting kind signature should not be displayed.
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "FFI_RedundantParenthesis"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataRedundantParenthesis"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "ClassRedundantParenthesis"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataHeadParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataTailParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataWholeParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataSelfParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "ClassSelfParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataKindAnnotation"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "DataKindAnnotationWithParens"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "FunctionParens1"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "FunctionParens2"
      , ShouldNotHaveKindSignature (n "KindSignatureDocs") "FunctionParens3"

      -- Declarations with no explicit kind signatures should be displayed
      -- if at least one type parameter has a kind other than `Type`
      -- despite all others having kind `Type`.
      , ShouldHaveKindSignature (n "KindSignatureDocs") "DShown" "data DShown :: Type -> Type -> (Type -> Type) -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "TShown" "type TShown :: (Type -> Type) -> Type -> Type -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "NShown" "newtype NShown :: Type -> (Type -> Type) -> Type -> Type"
      , ShouldHaveKindSignature (n "KindSignatureDocs") "CShown" "class CShown :: (Type -> Type) -> Type -> Type -> Constraint"
      ]
    )
  , ("RoleAnnotationDocs",
      [ ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "D_RNP" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "D_NPR" [P.Nominal, P.Phantom, P.Representational]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "D_PRN" [P.Phantom, P.Representational, P.Nominal]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_NNN" [P.Nominal, P.Nominal, P.Nominal]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_RNP" [P.Representational, P.Nominal, P.Phantom]

      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_Higher1" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_Higher2" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_Higher3" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_Higher4" [P.Representational, P.Nominal, P.Phantom]

      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_HeadParens" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_TailParens" [P.Representational, P.Nominal, P.Phantom]
      , ShouldHaveRoleAnnotation (n "RoleAnnotationDocs") "FFI_WholeParens" [P.Representational, P.Nominal, P.Phantom]
      ]
    )
  , ("DocCommentsMerge",
      [ ShouldMergeDocComments (n "DocCommentsMerge") "DataOnly" $ Just "decl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyData" $ Just "kind\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindAndData" $ Just "kind\n\ndecl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "DataRoleOnly" $ Just "role\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "DataAndRole" $ Just "decl\n\nrole\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyDataRoleOnly" $ Just "kind\n\nrole\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindDataAndRole" $ Just "kind\n\ndecl\n\nrole\n"

      , ShouldMergeDocComments (n "DocCommentsMerge") "FFIOnly" $ Just "decl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "FFIRoleOnly" $ Just "role\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "FFIAndRole" $ Just "decl\n\nrole\n"

      , ShouldMergeDocComments (n "DocCommentsMerge") "NewtypeOnly" $ Just "decl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyNewtype" $ Just "kind\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindAndNewtype" $ Just "kind\n\ndecl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "NewtypeRoleOnly" $ Just "role\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "NewtypeAndRole" $ Just "decl\n\nrole\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyNewtypeRoleOnly" $ Just "kind\n\nrole\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindNewtypeAndRole" $ Just "kind\n\ndecl\n\nrole\n"

      , ShouldMergeDocComments (n "DocCommentsMerge") "TypeOnly" $ Just "decl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyType" $ Just "kind\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindAndType" $ Just "kind\n\ndecl\n"

      , ShouldMergeDocComments (n "DocCommentsMerge") "ClassOnly" $ Just "decl\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindOnlyClass" $ Just "kind\n"
      , ShouldMergeDocComments (n "DocCommentsMerge") "KindAndClass" $ Just "kind\n\ndecl\n"
      ]
    )
  , ("Shebang1Undocumented",
      [ ShouldHaveModuleDocs (n "Shebang1Undocumented") Nothing
      ]
    )
  , ("Shebang2Undocumented",
      [ ShouldHaveModuleDocs (n "Shebang2Undocumented") Nothing
      ]
    )
  , ("Shebang3Undocumented",
      [ ShouldHaveModuleDocs (n "Shebang3Undocumented") $ Just "Normal doc comment\n"
      ]
    )
  , ("Shebang4Undocumented",
      [ ShouldHaveModuleDocs (n "Shebang4Undocumented") $ Just "Normal doc comment\n"
      ]
    )
  ]

  where
  n = P.moduleNameFromString
  pkg str = let Right p = parsePackageName str in p

  hasTypeVar varName =
    getAny . P.everythingOnTypes (<>) (Any . isVar varName)

  isVar varName (P.TypeVar _ name) | varName == T.unpack name = True
  isVar _ _ = False

  renderedType expected ty =
    codeToString (Docs.renderType ty) == expected

  shouldBeOrdered mn declNames =
    zipWith (ShouldComeBefore mn) declNames (tail declNames)

testTagsCases :: [(Text, [TagsAssertion])]
testTagsCases =
  [ ("DeclOrder",
      [ -- explicit exports
        ShouldBeTagged "x1" 10
      , ShouldBeTagged "x3" 11
      , ShouldBeTagged "X2" 13
      , ShouldBeTagged "X4" 14
      , ShouldBeTagged "A" 16
      , ShouldBeTagged "B" 17
      ])
  , ("Example2",
      [ -- all symbols exported
        ShouldBeTagged "one" 3
      , ShouldBeTagged "two" 6
      ])
  , ("ExplicitExport",
      [ -- only one of two symbols exported
        ShouldBeTagged "one" 3
      , ShouldNotBeTagged "two"
      ])
  ]

showQual :: P.ModuleName -> Text -> Text
showQual mn decl =
  P.runModuleName mn <> "." <> decl

showInPkg :: (a -> Text) -> Docs.InPackage a -> Text
showInPkg f = \case
  Docs.Local x ->
    f x <> " (local)"
  Docs.FromDep pkgName x ->
    f x <> " (from dep: " <> runPackageName pkgName <> ")"
