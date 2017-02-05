module Language.PureScript.Publish.ErrorsWarnings
  ( PackageError(..)
  , PackageWarning(..)
  , UserError(..)
  , InternalError(..)
  , OtherError(..)
  , RepositoryFieldError(..)
  , JSONSource(..)
  , printError
  , printErrorToStdout
  , renderError
  , printWarnings
  , renderWarnings
  ) where

import Prelude.Compat

import Control.Exception (IOException)

import Data.Aeson.BetterErrors (ParseError, displayError)
import Data.List (intersperse, intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Version
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.Publish.BoxesHelpers
import qualified Language.PureScript as P

import Web.Bower.PackageMeta (BowerError, PackageName, runPackageName, showBowerError)
import qualified Web.Bower.PackageMeta as Bower

-- | An error which meant that it was not possible to retrieve metadata for a
-- package.
data PackageError
  = UserError UserError
  | InternalError InternalError
  | OtherError OtherError
  deriving (Show)

data PackageWarning
  = NoResolvedVersion PackageName
  | UndeclaredDependency PackageName
  | UnacceptableVersion (PackageName, Text)
  | DirtyWorkingTree_Warn
  deriving (Show)

-- | An error that should be fixed by the user.
data UserError
  = BowerJSONNotFound
  | BowerExecutableNotFound [String] -- list of executable names tried
  | CouldntDecodeBowerJSON (ParseError BowerError)
  | TagMustBeCheckedOut
  | AmbiguousVersions [Version] -- Invariant: should contain at least two elements
  | BadRepositoryField RepositoryFieldError
  | NoLicenseSpecified
  | InvalidLicense
  | MissingDependencies (NonEmpty PackageName)
  | CompileError P.MultipleErrors
  | DirtyWorkingTree
  deriving (Show)

data RepositoryFieldError
  = RepositoryFieldMissing
  | BadRepositoryType Text
  | NotOnGithub
  deriving (Show)


-- | An error that probably indicates a bug in this module.
data InternalError
  = JSONError JSONSource (ParseError BowerError)
  | CouldntParseGitTagDate Text
  deriving (Show)

data JSONSource
  = FromFile FilePath
  | FromBowerList
  deriving (Show)

data OtherError
  = ProcessFailed String [String] IOException
  | IOExceptionThrown IOException
  deriving (Show)

printError :: PackageError -> IO ()
printError = printToStderr . renderError

printErrorToStdout :: PackageError -> IO ()
printErrorToStdout = printToStdout . renderError

renderError :: PackageError -> Box
renderError err =
  case err of
    UserError e ->
      vcat
        [ para (
          "There is a problem with your package, which meant that " ++
          "it could not be published."
          )
        , para "Details:"
        , indented (displayUserError e)
        ]
    InternalError e ->
      vcat
        [ para "Internal error: this is probably a bug. Please report it:"
        , indented (para "https://github.com/purescript/purescript/issues/new")
        , spacer
        , para "Details:"
        , successivelyIndented (displayInternalError e)
        ]
    OtherError e ->
      vcat
        [ para "An error occurred, and your package could not be published."
        , para "Details:"
        , indented (displayOtherError e)
        ]

displayUserError :: UserError -> Box
displayUserError e = case e of
  BowerJSONNotFound ->
    para (
      "The bower.json file was not found. Please create one, or run " ++
      "`pulp init`."
      )
  BowerExecutableNotFound names ->
    para (concat
      [ "The Bower executable was not found (tried: ", format names, "). Please"
      , " ensure that bower is installed and on your PATH."
      ])
    where
    format = intercalate ", " . map show
  CouldntDecodeBowerJSON err ->
    vcat
      [ para "There was a problem with your bower.json file:"
      , indented (vcat (map (para . T.unpack) (displayError showBowerError err)))
      , spacer
      , para "Please ensure that your bower.json file is valid."
      ]
  TagMustBeCheckedOut ->
      vcat
        [ para (concat
            [ "psc-publish requires a tagged version to be checked out in "
            , "order to build documentation, and no suitable tag was found. "
            , "Please check out a previously tagged version, or tag a new "
            , "version."
            ])
        , spacer
        , para "Note: tagged versions must be in the form"
        , indented (para "v{MAJOR}.{MINOR}.{PATCH} (example: \"v1.6.2\")")
        , spacer
        , para (concat
           [ "If the version you are publishing is not yet tagged, you might "
           , "want to use the --dry-run flag instead, which removes this "
           , "requirement. Run psc-publish --help for more details."
           ])
        ]
  AmbiguousVersions vs ->
    vcat $
      [ para (concat
          [ "The currently checked out commit seems to have been tagged with "
          , "more than 1 version, and I don't know which one should be used. "
          , "Please either delete some of the tags, or create a new commit "
          , "to tag the desired verson with."
          ])
      , spacer
      , para "Tags for the currently checked out commit:"
      ] ++ bulletedList showVersion vs
  BadRepositoryField err ->
    displayRepositoryError err
  NoLicenseSpecified ->
    vcat $
      [ para (concat
          [ "No license is specified in bower.json. Please add one, using the "
          , "SPDX license expression format. For example, any of the "
          , "following would be acceptable:"
          ])
      , spacer
      ] ++ spdxExamples ++
      [ spacer
      , para (
          "Note that distributing code without a license means that nobody "
          ++ "will (legally) be able to use it."
          )
      , spacer
      , para (concat
          [ "It is also recommended to add a LICENSE file to the repository, "
          , "including your name and the current year, although this is not "
          , "necessary."
          ])
      ]
  InvalidLicense ->
    vcat $
      [ para (concat
          [ "The license specified in bower.json is not a valid SPDX license "
          , "expression. Please use the SPDX license expression format. For "
          , "example, any of the following would be acceptable:"
          ])
      , spacer
      ] ++
      spdxExamples
  MissingDependencies pkgs ->
    let singular = NonEmpty.length pkgs == 1
        pl a b = if singular then b else a
        do_          = pl "do" "does"
        dependencies = pl "dependencies" "dependency"
        them         = pl "them" "it"
    in vcat $
      [ para (concat
        [ "The following Bower ", dependencies, " ", do_, " not appear to be "
        , "installed:"
        ])
      ] ++
        bulletedListT runPackageName (NonEmpty.toList pkgs)
        ++
      [ spacer
      , para (concat
        [ "Please install ", them, " first, by running `bower install`."
        ])
      ]
  CompileError err ->
    vcat
      [ para "Compile error:"
      , indented (vcat (P.prettyPrintMultipleErrorsBox P.defaultPPEOptions err))
      ]
  DirtyWorkingTree ->
    para (
        "Your git working tree is dirty. Please commit, discard, or stash " ++
        "your changes first."
        )

spdxExamples :: [Box]
spdxExamples =
  map (indented . para)
    [ "* \"MIT\""
    , "* \"Apache-2.0\""
    , "* \"BSD-2-Clause\""
    , "* \"GPL-2.0+\""
    , "* \"(GPL-3.0 OR MIT)\""
    ]

displayRepositoryError :: RepositoryFieldError -> Box
displayRepositoryError err = case err of
  RepositoryFieldMissing ->
    vcat
      [ para (concat
         [ "The 'repository' field is not present in your bower.json file. "
         , "Without this information, Pursuit would not be able to generate "
         , "source links in your package's documentation. Please add one - like "
         , "this, for example:"
         ])
      , spacer
      , indented (vcat
          [ para "\"repository\": {"
          , indented (para "\"type\": \"git\",")
          , indented (para "\"url\": \"git://github.com/purescript/purescript-prelude.git\"")
          , para "}"
          ]
        )
      ]
  BadRepositoryType ty ->
    para (concat
      [ "In your bower.json file, the repository type is currently listed as "
      , "\"" ++ T.unpack ty ++ "\". Currently, only git repositories are supported. "
      , "Please publish your code in a git repository, and then update the "
      , "repository type in your bower.json file to \"git\"."
      ])
  NotOnGithub ->
    vcat
      [ para (concat
        [ "The repository url in your bower.json file does not point to a "
        , "GitHub repository. Currently, Pursuit does not support packages "
        , "which are not hosted on GitHub."
        ])
      , spacer
      , para (concat
        [ "Please update your bower.json file to point to a GitHub repository. "
        , "Alternatively, if you would prefer not to host your package on "
        , "GitHub, please open an issue:"
        ])
      , indented (para "https://github.com/purescript/purescript/issues/new")
      ]

displayInternalError :: InternalError -> [String]
displayInternalError e = case e of
  JSONError src r ->
    [ "Error in JSON " ++ displayJSONSource src ++ ":"
    , T.unpack (Bower.displayError r)
    ]
  CouldntParseGitTagDate tag ->
    [ "Unable to parse the date for a git tag: " ++ T.unpack tag
    ]

displayJSONSource :: JSONSource -> String
displayJSONSource s = case s of
  FromFile fp ->
    "in file " ++ show fp
  FromBowerList ->
    "in the output of `bower list --json --offline`"

displayOtherError :: OtherError -> Box
displayOtherError e = case e of
  ProcessFailed prog args exc ->
    successivelyIndented
      [ "While running `" ++ prog ++ " " ++ unwords args ++ "`:"
      , show exc
      ]
  IOExceptionThrown exc ->
    successivelyIndented
      [ "An IO exception occurred:", show exc ]

data CollectedWarnings = CollectedWarnings
  { noResolvedVersions     :: [PackageName]
  , undeclaredDependencies :: [PackageName]
  , unacceptableVersions   :: [(PackageName, Text)]
  , dirtyWorkingTree       :: Any
  }
  deriving (Show, Eq, Ord)

instance Monoid CollectedWarnings where
  mempty = CollectedWarnings mempty mempty mempty mempty
  mappend (CollectedWarnings as bs cs d)
          (CollectedWarnings as' bs' cs' d') =
    CollectedWarnings (as <> as') (bs <> bs') (cs <> cs') (d <> d')

collectWarnings :: [PackageWarning] -> CollectedWarnings
collectWarnings = foldMap singular
  where
  singular w = case w of
    NoResolvedVersion    pn -> CollectedWarnings [pn] mempty mempty mempty
    UndeclaredDependency pn -> CollectedWarnings mempty [pn] mempty mempty
    UnacceptableVersion t   -> CollectedWarnings mempty mempty [t] mempty
    DirtyWorkingTree_Warn   -> CollectedWarnings mempty mempty mempty (Any True)

renderWarnings :: [PackageWarning] -> Box
renderWarnings warns =
  let CollectedWarnings{..} = collectWarnings warns
      go toBox warns' = toBox <$> NonEmpty.nonEmpty warns'
      mboxes = [ go warnNoResolvedVersions     noResolvedVersions
               , go warnUndeclaredDependencies undeclaredDependencies
               , go warnUnacceptableVersions   unacceptableVersions
               , if getAny dirtyWorkingTree
                   then Just warnDirtyWorkingTree
                   else Nothing
               ]
  in case catMaybes mboxes of
       []    -> nullBox
       boxes -> vcat [ para "Warnings:"
                     , indented (vcat (intersperse spacer boxes))
                     ]

warnNoResolvedVersions :: NonEmpty PackageName -> Box
warnNoResolvedVersions pkgNames =
  let singular = NonEmpty.length pkgNames == 1
      pl a b = if singular then b else a

      packages   = pl "packages" "package"
      anyOfThese = pl "any of these" "this"
      these      = pl "these" "this"
  in vcat $
    [ para (concat
      ["The following ", packages, " did not appear to have a resolved "
      , "version:"])
    ] ++
      bulletedListT runPackageName (NonEmpty.toList pkgNames)
      ++
    [ spacer
    , para (concat
      ["Links to types in ", anyOfThese, " ", packages, " will not work. In "
      , "order to make links work, edit your bower.json to specify a version"
      , " or a version range for ", these, " ", packages, ", and rerun "
      , "`bower install`."
      ])
    ]

warnUndeclaredDependencies :: NonEmpty PackageName -> Box
warnUndeclaredDependencies pkgNames =
  let singular = NonEmpty.length pkgNames == 1
      pl a b = if singular then b else a

      packages     = pl "packages" "package"
      are          = pl "are" "is"
      dependencies = pl "dependencies" "a dependency"
  in vcat $
    para (concat
      [ "The following Bower ", packages, " ", are, " installed, but not "
      , "declared as ", dependencies, " in your bower.json file:"
      ])
    : bulletedListT runPackageName (NonEmpty.toList pkgNames)

warnUnacceptableVersions :: NonEmpty (PackageName, Text) -> Box
warnUnacceptableVersions pkgs =
  let singular = NonEmpty.length pkgs == 1
      pl a b = if singular then b else a

      packages'  = pl "packages'" "package's"
      packages   = pl "packages" "package"
      anyOfThese = pl "any of these" "this"
      these      = pl "these" "this"
      versions   = pl "versions" "version"
  in vcat $
    [ para (concat
      [ "The following installed Bower ", packages', " ", versions, " could "
      , "not be parsed:"
      ])
    ] ++
      bulletedListT showTuple (NonEmpty.toList pkgs)
      ++
    [ spacer
    , para (concat
      ["Links to types in ", anyOfThese, " ", packages, " will not work. In "
      , "order to make links work, edit your bower.json to specify an "
      , "acceptable version or version range for ", these, " ", packages, ", "
      , "and rerun `bower install`."
      ])
    ]
  where
  showTuple (pkgName, tag) = runPackageName pkgName <> "#" <> tag

warnDirtyWorkingTree :: Box
warnDirtyWorkingTree =
  para (
    "Your working tree is dirty. (Note: this would be an error if it "
    ++ "were not a dry run)"
    )

printWarnings :: [PackageWarning] -> IO ()
printWarnings = printToStderr . renderWarnings
