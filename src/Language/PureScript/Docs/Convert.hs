{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Functions for converting PureScript ASTs into values of the data types
-- from Language.PureScript.Docs.

module Language.PureScript.Docs.Convert
  ( convertModule
  , collectBookmarks
  ) where

import Control.Monad
import Control.Category ((>>>))
import Data.Either
import Data.Maybe (mapMaybe, isNothing)
import Data.List (nub, isPrefixOf, isSuffixOf)

import qualified Language.PureScript as P

import Language.PureScript.Docs.Types

-- |
-- Convert a single Module.
--
convertModule :: P.Module -> Module
convertModule m@(P.Module _ coms moduleName  _ _) =
  Module (P.runModuleName moduleName) comments (declarations m)
  where
  comments = convertComments coms
  declarations =
    P.exportedDeclarations
    >>> mapMaybe (\d -> getDeclarationTitle d >>= convertDeclaration d)
    >>> augmentDeclarations
    >>> map addDefaultFixity

-- | The data type for an intermediate stage which we go through during
-- converting.
--
-- In the first pass, we take all top level declarations in the module, and
-- collect other information which will later be used to augment the top level
-- declarations. These two situation correspond to the Right and Left
-- constructors, respectively.
--
-- In the second pass, we go over all of the Left values and augment the
-- relevant declarations, leaving only the augmented Right values.
--
-- Note that in the Left case, we provide a [String] as well as augment
-- information. The [String] value should be a list of titles of declarations
-- that the augmentation should apply to. For example, for a type instance
-- declaration, that would be any types or type classes mentioned in the
-- instance. For a fixity declaration, it would be just the relevant operator's
-- name.
type IntermediateDeclaration
  = Either ([String], DeclarationAugment) Declaration

-- | Some data which will be used to augment a Declaration in the
-- output.
--
-- The AugmentChild constructor allows us to move all children under their
-- respective parents. It is only necessary for type instance declarations,
-- since they appear at the top level in the AST, and since they might need to
-- appear as children in two places (for example, if a data type defined in a
-- module is an instance of a type class also defined in that module).
--
-- The AugmentFixity constructor allows us to augment operator definitions
-- with their associativity and precedence.
data DeclarationAugment
  = AugmentChild ChildDeclaration
  | AugmentFixity P.Fixity

-- | Augment top-level declarations; the second pass. See the comments under
-- the type synonym IntermediateDeclaration for more information.
augmentDeclarations :: [IntermediateDeclaration] -> [Declaration]
augmentDeclarations (partitionEithers -> (augments, toplevels)) =
  foldl go toplevels augments
  where
  go ds (parentTitles, a) =
    map (\d ->
      if declTitle d `elem` parentTitles
        then augmentWith a d
        else d) ds

  augmentWith a d =
    case a of
      AugmentChild child ->
        d { declChildren = declChildren d ++ [child] }
      AugmentFixity fixity ->
        d { declFixity = Just fixity }

-- | Add the default operator fixity for operators which do not have associated
-- fixity declarations.
--
-- TODO: This may no longer be necessary after issue 806 is resolved, hopefully
-- in 0.8.
addDefaultFixity :: Declaration -> Declaration
addDefaultFixity decl@Declaration{..}
  | isOp declTitle && isNothing declFixity =
        decl { declFixity = Just defaultFixity }
  | otherwise =
        decl
  where
  isOp :: String -> Bool
  isOp str = "(" `isPrefixOf` str && ")" `isSuffixOf` str
  defaultFixity = P.Fixity P.Infixl (-1)

getDeclarationTitle :: P.Declaration -> Maybe String
getDeclarationTitle (P.TypeDeclaration name _)               = Just (P.showIdent name)
getDeclarationTitle (P.ExternDeclaration name _)             = Just (P.showIdent name)
getDeclarationTitle (P.DataDeclaration _ name _ _)           = Just (P.runProperName name)
getDeclarationTitle (P.ExternDataDeclaration name _)         = Just (P.runProperName name)
getDeclarationTitle (P.TypeSynonymDeclaration name _ _)      = Just (P.runProperName name)
getDeclarationTitle (P.TypeClassDeclaration name _ _ _)      = Just (P.runProperName name)
getDeclarationTitle (P.TypeInstanceDeclaration name _ _ _ _) = Just (P.showIdent name)
getDeclarationTitle (P.FixityDeclaration _ name)             = Just ("(" ++ name ++ ")")
getDeclarationTitle (P.PositionedDeclaration _ _ d)          = getDeclarationTitle d
getDeclarationTitle _                                        = Nothing

-- | Create a basic Declaration value.
mkDeclaration :: String -> DeclarationInfo -> Declaration
mkDeclaration title info =
  Declaration { declTitle      = title
              , declComments   = Nothing
              , declSourceSpan = Nothing
              , declChildren   = []
              , declFixity     = Nothing
              , declInfo       = info
              }

basicDeclaration :: String -> DeclarationInfo -> Maybe IntermediateDeclaration
basicDeclaration title info = Just $ Right $ mkDeclaration title info

convertDeclaration :: P.Declaration -> String -> Maybe IntermediateDeclaration
convertDeclaration (P.TypeDeclaration _ ty) title =
  basicDeclaration title (ValueDeclaration ty)
convertDeclaration (P.ExternDeclaration _ ty) title =
  basicDeclaration title (ValueDeclaration ty)
convertDeclaration (P.DataDeclaration dtype _ args ctors) title =
  Just (Right (mkDeclaration title info) { declChildren = children })
  where
  info = DataDeclaration dtype args
  children = map convertCtor ctors
  convertCtor (ctor', tys) =
    ChildDeclaration (P.runProperName ctor') Nothing Nothing (ChildDataConstructor tys)
convertDeclaration (P.ExternDataDeclaration _ kind') title =
  basicDeclaration title (ExternDataDeclaration kind')
convertDeclaration (P.TypeSynonymDeclaration _ args ty) title =
  basicDeclaration title (TypeSynonymDeclaration args ty)
convertDeclaration (P.TypeClassDeclaration _ args implies ds) title =
  Just (Right (mkDeclaration title info) { declChildren = children })
  where
  info = TypeClassDeclaration args implies
  children = map convertClassMember ds
  convertClassMember (P.PositionedDeclaration _ _ d) =
    convertClassMember d
  convertClassMember (P.TypeDeclaration ident' ty) =
    ChildDeclaration (P.showIdent ident') Nothing Nothing (ChildTypeClassMember ty)
  convertClassMember _ =
    P.internalError "convertDeclaration: Invalid argument to convertClassMember."
convertDeclaration (P.TypeInstanceDeclaration _ constraints className tys _) title =
  Just (Left (classNameString : typeNameStrings, AugmentChild childDecl))
  where
  classNameString = unQual className
  typeNameStrings = nub (concatMap (P.everythingOnTypes (++) extractProperNames) tys)
  unQual x = let (P.Qualified _ y) = x in P.runProperName y

  extractProperNames (P.TypeConstructor n) = [unQual n]
  extractProperNames _ = []

  childDecl = ChildDeclaration title Nothing Nothing (ChildInstance constraints classApp)
  classApp = foldl P.TypeApp (P.TypeConstructor className) tys
convertDeclaration (P.FixityDeclaration fixity _) title =
  Just (Left ([title], AugmentFixity fixity))
convertDeclaration (P.PositionedDeclaration srcSpan com d') title =
  fmap (addComments . addSourceSpan) (convertDeclaration d' title)
  where
  addComments (Right d) =
    Right (d { declComments = convertComments com })
  addComments (Left augment) =
    Left (withAugmentChild (\d -> d { cdeclComments = convertComments com })
                           augment)

  addSourceSpan (Right d) =
    Right (d { declSourceSpan = Just srcSpan })
  addSourceSpan (Left augment) =
    Left (withAugmentChild (\d -> d { cdeclSourceSpan = Just srcSpan })
                           augment)

  withAugmentChild f (t, a) =
    case a of
      AugmentChild d -> (t, AugmentChild (f d))
      _              -> (t, a)
convertDeclaration _ _ = Nothing

convertComments :: [P.Comment] -> Maybe String
convertComments cs = do
  let raw = concatMap toLines cs
  guard (all hasPipe raw && not (null raw))
  return (go raw)
  where
  go = unlines . map stripPipes

  toLines (P.LineComment s) = [s]
  toLines (P.BlockComment s) = lines s

  hasPipe s = case dropWhile (== ' ') s of { ('|':_) -> True; _ -> False }

  stripPipes = dropPipe . dropWhile (== ' ')

  dropPipe ('|':' ':s) = s
  dropPipe ('|':s) = s
  dropPipe s = s

-- | Go through a PureScript module and extract a list of Bookmarks; references
-- to data types or values, to be used as a kind of index. These are used for
-- generating links in the HTML documentation, for example.
collectBookmarks :: InPackage P.Module -> [Bookmark]
collectBookmarks (Local m) = map Local (collectBookmarks' m)
collectBookmarks (FromDep pkg m) = map (FromDep pkg) (collectBookmarks' m)

collectBookmarks' :: P.Module -> [(P.ModuleName, String)]
collectBookmarks' m =
  map (P.getModuleName m, )
      (mapMaybe getDeclarationTitle
                (P.exportedDeclarations m))
