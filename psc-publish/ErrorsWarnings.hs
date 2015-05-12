{-# LANGUAGE OverloadedStrings #-}

module ErrorsWarnings where

import Data.Aeson.BetterErrors
import Data.Version
import Data.Maybe
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Text as T

import Control.Exception (IOException)
import Web.Bower.PackageMeta (BowerError, PackageName, runPackageName)
import qualified Web.Bower.PackageMeta as Bower

import qualified Language.PureScript as P
import qualified Language.PureScript.Docs as D

import BoxesHelpers

-- | An error which meant that it was not possible to retrieve metadata for a
-- package.
data PackageError
  = UserError UserError
  | InternalError InternalError
  | OtherError OtherError
  deriving (Show)

data PackageWarning
  = ResolutionNotVersion PackageName
  | UndeclaredDependency PackageName
  deriving (Show)

-- | An error that should be fixed by the user.
data UserError
  = BowerJSONNotFound
  | CouldntParseBowerJSON (ParseError BowerError)
  | BowerJSONNameMissing
  | TagMustBeCheckedOut
  | AmbiguousVersions [Version] -- Invariant: should contain at least two elements
  | BadRepositoryField RepositoryFieldError
  | MissingDependencies (NonEmpty PackageName)
  | ParseAndDesugarError D.ParseDesugarError
  deriving (Show)

data RepositoryFieldError
  = RepositoryFieldMissing
  | BadRepositoryType String
  | NotOnGithub
  deriving (Show)

-- | An error that probably indicates a bug in this module.
data InternalError
  = JSONError JSONSource (ParseError BowerError)
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

renderError :: PackageError -> Box
renderError err =
  case err of
    UserError e ->
      vcat
        [ para (concat
          [ "There is a problem with your package, which meant that "
          , "it could not be published."
          ])
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
    para (concat
      [ "The bower.json file was not found. Please create one, or run "
      , "`pulp init`."
      ])
  CouldntParseBowerJSON err ->
    vcat
      [ successivelyIndented
        [ "The bower.json file could not be parsed as JSON:"
        , "aeson reported: " ++ show err
        ]
      , para "Please ensure that your bower.json file is valid JSON."
      ]
  BowerJSONNameMissing ->
    vcat
      [ successivelyIndented
        [ "In bower.json:"
        , "the \"name\" key was not found."
        ]
      , para "Please give your package a name first."
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
        , para "Note: tagged versions must be in one of the following forms:"
        , indented (para "* v{MAJOR}.{MINOR}.{PATCH} (example: \"v1.6.2\")")
        , indented (para "* {MAJOR}.{MINOR}.{PATCH} (example: \"1.6.2\")")
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
        bulletedList runPackageName (NonEmpty.toList pkgs)
        ++
      [ spacer
      , para (concat
        [ "Please install ", them, " first, by running `bower install`."
        ])
      ]
  ParseAndDesugarError (D.ParseError err) ->
    vcat
      [ para "Parse error:"
      , indented (para (show err))
      ]
  ParseAndDesugarError (D.SortModulesError err) ->
    vcat
      [ para "Error in sortModules:"
      , indented (para (P.prettyPrintMultipleErrors False err))
      ]
  ParseAndDesugarError (D.DesugarError err) ->
    vcat
      [ para "Error while desugaring:"
      , indented (para (P.prettyPrintMultipleErrors False err))
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
      , "\"" ++ ty ++ "\". Currently, only git repositories are supported. "
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

renderWarnings :: [PackageWarning] -> Box
renderWarnings =
  collectWarnings
    [ (getResolutionNotVersion, warnResolutionNotVersions)
    , (getUndeclaredDependency, warnUndeclaredDependencies)
    ]
  where
  collectWarnings patterns warns =
    let boxes = mapMaybe (collectWarnings' warns) patterns
        result = vcat
                    [ para "Warnings:"
                    , indented (vcat (intersperse spacer boxes))
                    ]
    in if null boxes then nullBox else result

  getResolutionNotVersion (ResolutionNotVersion n) = Just n
  getResolutionNotVersion _ = Nothing

  getUndeclaredDependency (UndeclaredDependency n) = Just n
  getUndeclaredDependency _ = Nothing

collectWarnings' :: [PackageWarning] -> ((PackageWarning -> Maybe a), (NonEmpty a -> Box)) -> Maybe Box
collectWarnings' warns (pattern, render) =
  case mapMaybe pattern warns of
    []     -> Nothing
    (x:xs) -> Just (render (x :| xs))

warnResolutionNotVersions :: NonEmpty PackageName -> Box
warnResolutionNotVersions pkgNames =
  let singular = NonEmpty.length pkgNames == 1
      pl a b = if singular then b else a

      packages   = pl "packages" "package"
      were       = pl "were" "was"
      anyOfThese = pl "any of these" "this"
      these      = pl "these" "this"
  in vcat $
    [ para (concat
      ["The following ", packages, " ", were, " not resolved to a version:"])
    ] ++
      bulletedList runPackageName (NonEmpty.toList pkgNames)
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
    [ para (concat
      [ "The following Bower ", packages, " ", are, " installed, but not "
      , "declared as ", dependencies, " in your bower.json file:"
      ])
    ] ++
      bulletedList runPackageName (NonEmpty.toList pkgNames)

printWarnings :: [PackageWarning] -> IO ()
printWarnings = printToStderr . renderWarnings
