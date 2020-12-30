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
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Monoid
import Data.Version
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as T

import qualified Language.PureScript.Docs.Types as D
import qualified Language.PureScript as P
import Language.PureScript.Publish.BoxesHelpers

import Web.Bower.PackageMeta (PackageName, runPackageName, showBowerError)

-- | An error which meant that it was not possible to retrieve metadata for a
-- package.
data PackageError
  = UserError UserError
  | InternalError InternalError
  | OtherError OtherError
  deriving (Show)

data PackageWarning
  = NoResolvedVersion PackageName
  | UnacceptableVersion (PackageName, Text)
  | DirtyWorkingTree_Warn
  deriving (Show)

-- | An error that should be fixed by the user.
data UserError
  = PackageManifestNotFound
  | ResolutionsFileNotFound
  | CouldntDecodePackageManifest (ParseError D.ManifestError)
  | TagMustBeCheckedOut
  | AmbiguousVersions [Version] -- Invariant: should contain at least two elements
  | BadRepositoryField RepositoryFieldError
  | NoLicenseSpecified
  | InvalidLicense
  | MissingDependencies (NonEmpty PackageName)
  | CompileError P.MultipleErrors
  | DirtyWorkingTree
  | ResolutionsFileError FilePath (ParseError D.PackageError)
  deriving (Show)

data RepositoryFieldError
  = RepositoryFieldMissing (Maybe Text)
  | BadRepositoryType Text
  | NotOnGithub
  deriving (Show)

-- | An error that probably indicates a bug in this module.
data InternalError
  = CouldntParseGitTagDate Text
  deriving (Show)

data JSONSource
  = FromFile FilePath
  | FromResolutions
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
  PackageManifestNotFound ->
    para (
      "The package manifest file was not found. Please create one, or run " ++
      "`pulp init`."
      )
  ResolutionsFileNotFound ->
    para "The resolutions file was not found."
  CouldntDecodePackageManifest err ->
    vcat
      [ para "There was a problem with your package manifest file:"
      , indented (vcat (map (para . T.unpack) (displayError showBowerError err)))
      , spacer
      , para "Please ensure that your package manifest file is valid."
      ]
  TagMustBeCheckedOut ->
      vcat
        [ para (concat
            [ "purs publish requires a tagged version to be checked out in "
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
           , "requirement. Run `purs publish --help` for more details."
           ])
        ]
  AmbiguousVersions vs ->
    vcat $
      [ para (concat
          [ "The currently checked out commit seems to have been tagged with "
          , "more than 1 version, and I don't know which one should be used. "
          , "Please either delete some of the tags, or create a new commit "
          , "to tag the desired version with."
          ])
      , spacer
      , para "Tags for the currently checked out commit:"
      ] ++ bulletedList showVersion vs
  BadRepositoryField err ->
    displayRepositoryError err
  NoLicenseSpecified ->
    vcat $
      [ para $ concat
          [ "No license is specified in package manifest. Please add a "
          , "\"license\" property with a SPDX license expression. For example, "
          , "any of the following would be acceptable:"
          ]
      , spacer
      ] ++ spdxExamples ++
      [ spacer
      , para $ concat
          [ "See https://spdx.org/licenses/ for a full list of licenses. For more "
          , "information on SPDX license expressions, see https://spdx.org/ids-how"
          ]
      , spacer
      , para $ concat
          [ "Note that distributing code without a license means that nobody will "
          , "(legally) be able to use it."
          ]
      , spacer
      , para $ concat
          [ "It is also recommended to add a LICENSE file to the repository, "
          , "including your name and the current year, although this is not necessary."
          ]
      ]
  InvalidLicense ->
    vcat $
      [ para $ concat
          [ "The license specified in package manifest is not a valid SPDX "
          , "license expression. Please update the \"license\" property so that "
          , "it is a valid SPDX license expression. For example, any of the "
          , "following would be acceptable:"
          ]
      , spacer
      ] ++
      spdxExamples
  MissingDependencies pkgs ->
    let singular = NonEmpty.length pkgs == 1
        pl a b = if singular then b else a
        do_          = pl "do" "does"
        dependencies = pl "dependencies" "dependency"
    in vcat $
      [ para (concat
        [ "The following ", dependencies, " ", do_, " not appear to be "
        , "installed:"
        ])
      ] ++
        bulletedListT runPackageName (NonEmpty.toList pkgs)
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
  ResolutionsFileError path err ->
    successivelyIndented $
      [ "Error in resolutions file (" ++ path ++ "):" ]
      ++ map T.unpack (displayError D.displayPackageError err)

spdxExamples :: [Box]
spdxExamples =
  map (indented . para)
    [ "* \"MIT\""
    , "* \"Apache-2.0\""
    , "* \"BSD-2-Clause\""
    , "* \"GPL-2.0-or-later\""
    , "* \"(GPL-3.0-only OR MIT)\""
    ]

displayRepositoryError :: RepositoryFieldError -> Box
displayRepositoryError err = case err of
  RepositoryFieldMissing giturl ->
    vcat
      [ para (concat
         [ "The 'repository' field is not present in your package manifest file. "
         , "Without this information, Pursuit would not be able to generate "
         , "source links in your package's documentation. Please add one - like "
         , "this, for example:"
         ])
      , spacer
      , indented (vcat
          [ para "\"repository\": {"
          , indented (para "\"type\": \"git\",")
          , indented (para ("\"url\": \"" ++ T.unpack (fromMaybe "https://github.com/USER/REPO.git" giturl) ++ "\""))
          , para "}"
          ]
        )
      ]
  BadRepositoryType ty ->
    para (concat
      [ "In your package manifest file, the repository type is currently listed as "
      , "\"" ++ T.unpack ty ++ "\". Currently, only git repositories are supported. "
      , "Please publish your code in a git repository, and then update the "
      , "repository type in your package manifest file to \"git\"."
      ])
  NotOnGithub ->
    vcat
      [ para (concat
        [ "The repository url in your package manifest file does not point to a "
        , "GitHub repository. Currently, Pursuit does not support packages "
        , "which are not hosted on GitHub."
        ])
      , spacer
      , para (concat
        [ "Please update your package manifest file to point to a GitHub repository. "
        , "Alternatively, if you would prefer not to host your package on "
        , "GitHub, please open an issue:"
        ])
      , indented (para "https://github.com/purescript/purescript/issues/new")
      ]

displayInternalError :: InternalError -> [String]
displayInternalError e = case e of
  CouldntParseGitTagDate tag ->
    [ "Unable to parse the date for a git tag: " ++ T.unpack tag
    ]

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
  { noResolvedVersions      :: [PackageName]
  , unacceptableVersions    :: [(PackageName, Text)]
  , dirtyWorkingTree        :: Any
  }
  deriving (Show, Eq, Ord)

instance Semigroup CollectedWarnings where
  (<>) (CollectedWarnings a b c) (CollectedWarnings a' b' c') =
    CollectedWarnings (a <> a') (b <> b') (c <> c')

instance Monoid CollectedWarnings where
  mempty = CollectedWarnings mempty mempty mempty

collectWarnings :: [PackageWarning] -> CollectedWarnings
collectWarnings = foldMap singular
  where
  singular w = case w of
    NoResolvedVersion pn ->
      mempty { noResolvedVersions = [pn] }
    UnacceptableVersion t ->
      mempty { unacceptableVersions = [t] }
    DirtyWorkingTree_Warn ->
      mempty { dirtyWorkingTree = Any True }

renderWarnings :: [PackageWarning] -> Box
renderWarnings warns =
  let CollectedWarnings{..} = collectWarnings warns
      go toBox warns' = toBox <$> NonEmpty.nonEmpty warns'
      mboxes = [ go warnNoResolvedVersions noResolvedVersions
               , go warnUnacceptableVersions unacceptableVersions
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
      , "order to make links work, edit your package manifest to specify a version"
      , " or a version range for ", these, " ", packages, "."
      ])
    ]

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
      [ "The following installed ", packages', " ", versions, " could "
      , "not be parsed:"
      ])
    ] ++
      bulletedListT showTuple (NonEmpty.toList pkgs)
      ++
    [ spacer
    , para (concat
      ["Links to types in ", anyOfThese, " ", packages, " will not work. In "
      , "order to make links work, edit your package manifest to specify an "
      , "acceptable version or version range for ", these, " ", packages, "."
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
