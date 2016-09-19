{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module TestDocs where

import Prelude ()
import Prelude.Compat

import Data.Version (Version(..))

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.List ((\\))
import Data.Foldable
import System.Exit

import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as Docs
import Language.PureScript.Docs.AsMarkdown (codeToString)
import qualified Language.PureScript.Publish as Publish
import qualified Language.PureScript.Publish.ErrorsWarnings as Publish

import TestUtils

publishOpts :: Publish.PublishOptions
publishOpts = Publish.defaultPublishOptions
  { Publish.publishGetVersion = return testVersion
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
        let mdl = takeJust ("module not found in docs: " ++ P.runModuleName mn)
                          (find ((==) mn . Docs.modName) pkgModules)
        in forM_ pragmas (`runAssertionIO` mdl)


takeJust :: String -> Maybe a -> a
takeJust msg = fromMaybe (error msg)

data Assertion
  -- | Assert that a particular declaration is documented with the given
  -- children
  = ShouldBeDocumented P.ModuleName String [String]
  -- | Assert that a particular declaration is not documented
  | ShouldNotBeDocumented P.ModuleName String
  -- | Assert that a particular declaration exists, but without a particular
  -- child.
  | ChildShouldNotBeDocumented P.ModuleName String String
  -- | Assert that a particular declaration has a particular type class
  -- constraint.
  | ShouldBeConstrained P.ModuleName String String
  -- | Assert that a particular value declaration exists, and its type
  -- satisfies the given predicate.
  | ValueShouldHaveTypeSignature P.ModuleName String (ShowFn (P.Type -> Bool))
  -- | Assert that a particular type alias exists, and its corresponding
  -- type, when rendered, matches a given string exactly
  -- fields: module, type synonym name, expected type
  | TypeSynonymShouldRenderAs P.ModuleName String String
  deriving (Show)

newtype ShowFn a = ShowFn a

instance Show (ShowFn a) where
  show _ = "<function>"

data AssertionFailure
  -- | A declaration was not documented, but should have been
  = NotDocumented P.ModuleName String
  -- | A child declaration was not documented, but should have been
  | ChildrenNotDocumented P.ModuleName String [String]
  -- | A declaration was documented, but should not have been
  | Documented P.ModuleName String
  -- | A child declaration was documented, but should not have been
  | ChildDocumented P.ModuleName String String
  -- | A constraint was missing.
  | ConstraintMissing P.ModuleName String String
  -- | A declaration had the wrong "type" (ie, value, type, type class)
  -- Fields: declaration title, expected "type", actual "type".
  | WrongDeclarationType P.ModuleName String String String
  -- | A value declaration had the wrong type (in the sense of "type
  -- checking"), eg, because the inferred type was used when the explicit type
  -- should have been.
  -- Fields: module name, declaration name, actual type.
  | ValueDeclarationWrongType P.ModuleName String P.Type
  -- | A Type synonym has been rendered in an unexpected format
  -- Fields: module name, declaration name, expected rendering, actual rendering
  | TypeSynonymMismatch P.ModuleName String String String
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
      case find ((==) decl . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just Docs.Declaration{..} ->
          case declInfo of
            Docs.ValueDeclaration ty ->
              if checkConstrained ty tyClass
                then Pass
                else Fail (ConstraintMissing mn decl tyClass)
            _ ->
              Fail (WrongDeclarationType mn decl "value"
                     (Docs.declInfoToString declInfo))

    ValueShouldHaveTypeSignature mn decl (ShowFn tyPredicate) ->
      case find ((==) decl . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just Docs.Declaration{..} ->
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
      case find ((==) decl . Docs.declTitle) (declarationsFor mn) of
        Nothing ->
          Fail (NotDocumented mn decl)
        Just Docs.Declaration{..} ->
          case declInfo of
            Docs.TypeSynonymDeclaration [] ty ->
              let actual = codeToString (Docs.renderType ty) in
              if actual == expected
                 then Pass
                 else Fail (TypeSynonymMismatch mn decl expected actual)
            _ ->
              Fail (WrongDeclarationType mn decl "synonym"
                    (Docs.declInfoToString declInfo))

  where
  declarationsFor mn =
    if mn == modName
      then modDeclarations
      else fromMaybe [] (lookup mn modReExports)

  findChildren title =
    fmap childrenTitles . find ((==) title . Docs.declTitle)

  childrenTitles = map Docs.cdeclTitle . Docs.declChildren

checkConstrained :: P.Type -> String -> Bool
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
  putStrLn ("In " ++ P.runModuleName (Docs.modName mdl) ++ ": " ++ show assertion)
  case runAssertion assertion mdl of
    Pass -> pure ()
    Fail reason -> do
      putStrLn ("Failed: " <> show reason)
      exitFailure

testCases :: [(String, [Assertion])]
testCases =
  [ ("Example",
      [ -- From dependencies
        ShouldBeDocumented    (n "Prelude") "Unit" []
      , ShouldNotBeDocumented (n "Prelude") "unit"

        -- From local files
      , ShouldBeDocumented    (n "Example2") "one" []
      , ShouldNotBeDocumented (n "Example2") "two"
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
  ]

  where
  n = P.moduleNameFromString

  hasTypeVar varName =
    getAny . P.everythingOnTypes (<>) (Any . isVar varName)

  isVar varName (P.TypeVar name) | varName == name = True
  isVar _ _ = False

  renderedType expected =
    ShowFn $ \ty -> codeToString (Docs.renderType ty) == expected
