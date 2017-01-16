{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TestDocs where

import Prelude ()
import Prelude.Compat

import Control.Arrow (first)
import Control.Monad.IO.Class (liftIO)

import Data.Foldable
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Version (Version(..))
import System.Exit

import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as Docs
import Language.PureScript.Docs.AsMarkdown (codeToString)
import qualified Language.PureScript.Publish as Publish
import qualified Language.PureScript.Publish.ErrorsWarnings as Publish

import Web.Bower.PackageMeta (parsePackageName)

import TestUtils

publishOpts :: Publish.PublishOptions
publishOpts = Publish.defaultPublishOptions
  { Publish.publishGetVersion = return testVersion
  , Publish.publishGetTagTime = const (liftIO getCurrentTime)
  , Publish.publishWorkingTreeDirty = return ()
  }
  where testVersion = ("v999.0.0", Version [999,0,0] [])

main :: IO ()
main = pushd "examples/docs" $ do
  res <- Publish.preparePackage publishOpts
  case res of
    Left e -> Publish.printErrorToStdout e >> exitFailure
    Right Docs.Package{..} ->
      forM_ testCases $ \(P.moduleNameFromString -> mn, pragmas) ->
        let mdl = takeJust ("module not found in docs: " ++ T.unpack (P.runModuleName mn))
                          (find ((==) mn . Docs.modName) pkgModules)
        in forM_ pragmas (`runAssertionIO` mdl)


takeJust :: String -> Maybe a -> a
takeJust msg = fromMaybe (error msg)

data Assertion
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
  | ValueShouldHaveTypeSignature P.ModuleName Text (ShowFn (P.Type -> Bool))
  -- | Assert that a particular type alias exists, and its corresponding
  -- type, when rendered, matches a given string exactly
  -- fields: module, type synonym name, expected type
  | TypeSynonymShouldRenderAs P.ModuleName Text Text
  -- | Assert that a documented declaration includes a documentation comment
  -- containing a particular string
  | ShouldHaveDocComment P.ModuleName Text Text
  -- | Assert that there should be some declarations re-exported from a
  -- particular module in a particular package.
  | ShouldHaveReExport (Docs.InPackage P.ModuleName)
  deriving (Show)

newtype ShowFn a = ShowFn a

instance Show (ShowFn a) where
  show _ = "<function>"

data AssertionFailure
  -- | A declaration was not documented, but should have been
  = NotDocumented P.ModuleName Text
  -- | A child declaration was not documented, but should have been
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
  -- | A value declaration had the wrong type (in the sense of "type
  -- checking"), eg, because the inferred type was used when the explicit type
  -- should have been.
  -- Fields: module name, declaration name, actual type.
  | ValueDeclarationWrongType P.ModuleName Text P.Type
  -- | A Type synonym has been rendered in an unexpected format
  -- Fields: module name, declaration name, expected rendering, actual rendering
  | TypeSynonymMismatch P.ModuleName Text Text Text
  -- | A doc comment was not found or did not match what was expected
  -- Fields: module name, expected substring, actual comments
  | DocCommentMissing P.ModuleName Text (Maybe Text)
  -- | A module was missing re-exports from a particular module.
  -- Fields: module name, expected re-export, actual re-exports.
  | ReExportMissing P.ModuleName (Docs.InPackage P.ModuleName) [Docs.InPackage P.ModuleName]
  deriving (Show)

data AssertionResult
  = Pass
  | Fail AssertionFailure
  deriving (Show)

runAssertion :: Assertion -> Docs.Module -> AssertionResult
runAssertion assertion Docs.Module{..} =
  case assertion of
    ShouldBeDocumented mn decl children ->
      case findChildren decl (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just actualChildren ->
          case children \\ actualChildren of
            [] -> Pass
            cs -> Fail (ChildrenNotDocumented mn decl cs)

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

    ValueShouldHaveTypeSignature mn decl (ShowFn tyPredicate) ->
      findDecl mn decl $ \Docs.Declaration{..} ->
        case declInfo of
          Docs.ValueDeclaration ty ->
            if tyPredicate ty
              then Pass
              else Fail
                (ValueDeclarationWrongType mn decl ty)
          _ ->
            Fail (WrongDeclarationType mn decl "value"
                   (Docs.declInfoToString declInfo))

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

    ShouldHaveReExport reExp ->
      let
        reExps = map fst modReExports
      in
        if reExp `elem` reExps
          then Pass
          else Fail (ReExportMissing modName reExp reExps)

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

  childrenTitles = map Docs.cdeclTitle . Docs.declChildren

checkConstrained :: P.Type -> Text -> Bool
checkConstrained ty tyClass =
  -- Note that we don't recurse on ConstrainedType if none of the constraints
  -- match; this is by design, as constraints should be moved to the front
  -- anyway.
  case ty of
    P.ConstrainedType cs _ | any (matches tyClass) cs ->
      True
    P.ForAll _ ty' _ ->
      checkConstrained ty' tyClass
    _ ->
      False
  where
  matches className =
    (==) className . P.runProperName . P.disqualify . P.constraintClass

runAssertionIO :: Assertion -> Docs.Module -> IO ()
runAssertionIO assertion mdl = do
  putStrLn ("In " ++ T.unpack (P.runModuleName (Docs.modName mdl)) ++ ": " ++ show assertion)
  case runAssertion assertion mdl of
    Pass -> pure ()
    Fail reason -> do
      putStrLn ("Failed: " <> show reason)
      exitFailure

testCases :: [(Text, [Assertion])]
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
      , ShouldBeDocumented (n "Clash1") "Type" []
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
      [ ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "explicit" (ShowFn (hasTypeVar "something"))
      , ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "anInt"    (ShowFn (P.tyInt ==))
      , ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "aNumber"  (ShowFn (P.tyNumber ==))
      , ValueShouldHaveTypeSignature (n "ExplicitTypeSignatures") "nestedForAll" (renderedType "forall c. (forall a b. c)")
      ])

  , ("ConstrainedArgument",
      [ TypeSynonymShouldRenderAs (n "ConstrainedArgument") "WithoutArgs" "forall a. (Partial => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "WithArgs" "forall a. (Foo a => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "MultiWithoutArgs" "forall a. ((Partial, Partial) => a) -> a"
      , TypeSynonymShouldRenderAs (n "ConstrainedArgument") "MultiWithArgs" "forall a b. ((Foo a, Foo b) => a) -> a"
      ])

  , ("TypeOpAliases",
      [ ValueShouldHaveTypeSignature (n "TypeOpAliases") "test1" (renderedType "forall a b. a ~> b")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test2" (renderedType "forall a b c. a ~> b ~> c")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test3" (renderedType "forall a b c d. a ~> (b ~> c) ~> d")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "test4" (renderedType "forall a b c d. ((a ~> b) ~> c) ~> d")
      , ValueShouldHaveTypeSignature (n "TypeOpAliases") "third" (renderedType "forall a b c. a × b × c -> c")
      ])

  , ("DocComments",
      [ ShouldHaveDocComment (n "DocComments") "example" "    example == 0"
      ])
  ]

  where
  n = P.moduleNameFromString . T.pack
  pkg str = let Right p = parsePackageName str in p

  hasTypeVar varName =
    getAny . P.everythingOnTypes (<>) (Any . isVar varName)

  isVar varName (P.TypeVar name) | varName == T.unpack name = True
  isVar _ _ = False

  renderedType expected =
    ShowFn $ \ty -> codeToString (Docs.renderType ty) == expected
