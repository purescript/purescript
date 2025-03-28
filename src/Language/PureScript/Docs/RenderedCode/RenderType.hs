-- HLint is confused by the identifier `pattern` if PatternSynonyms is enabled.
{-# LANGUAGE NoPatternSynonyms #-}

-- | Functions for producing RenderedCode values from PureScript Type values.

module Language.PureScript.Docs.RenderedCode.RenderType
  ( renderType
  , renderTypeWithRole
  , renderType'
  , renderTypeAtom
  , renderTypeAtom'
  , renderRow
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.List (uncons)

import Control.Arrow ((<+>))
import Control.PatternArrows as PA

import Language.PureScript.Crash (internalError)
import Language.PureScript.Label (Label)
import Language.PureScript.Names (coerceProperName)
import Language.PureScript.Pretty.Types (PrettyPrintConstraint, PrettyPrintType(..), convertPrettyPrintType, prettyPrintLabel)
import Language.PureScript.Roles (Role, displayRole)
import Language.PureScript.Types (Type, TypeVarVisibility, typeVarVisibilityPrefix)
import Language.PureScript.PSString (prettyPrintString)

import Language.PureScript.Docs.RenderedCode.Types (RenderedCode, keywordForall, roleAnn, sp, syntax, typeCtor, typeOp, typeVar)
import Language.PureScript.Docs.Utils.MonoidExtras (mintersperse)

typeLiterals :: Pattern () PrettyPrintType RenderedCode
typeLiterals = mkPattern match
  where
  match (PPTypeWildcard name) =
    Just $ syntax $ maybe "_" ("?" <>) name
  match (PPTypeVar var role) =
    Just $ typeVar var <> roleAnn role
  match (PPRecord labels tail_) =
    Just $ mintersperse sp
              [ syntax "{"
              , renderRow labels tail_
              , syntax "}"
              ]
  match (PPTypeConstructor n) =
    Just (typeCtor n)
  match (PPRow labels tail_) =
    Just (syntax "(" <> renderRow labels tail_ <> syntax ")")
  match (PPBinaryNoParensType op l r) =
    Just $ renderTypeAtom' l <> sp <> renderTypeAtom' op <> sp <> renderTypeAtom' r
  match (PPTypeOp n) =
    Just (typeOp n)
  match (PPTypeLevelString str) =
    Just (syntax (prettyPrintString str))
  match (PPTypeLevelInt nat) =
    Just (syntax $ pack $ show nat)
  match _ =
    Nothing

renderConstraint :: PrettyPrintConstraint -> RenderedCode
renderConstraint (pn, ks, tys) =
  let instApp = foldl PPTypeApp (foldl (\a b -> PPTypeApp a (PPKindArg b)) (PPTypeConstructor (fmap coerceProperName pn)) ks) tys
  in  renderType' instApp

renderConstraints :: PrettyPrintConstraint -> RenderedCode -> RenderedCode
renderConstraints con ty =
  mintersperse sp
    [ renderConstraint con
    , syntax "=>"
    , ty
    ]

-- |
-- Render code representing a Row
--
renderRow :: [(Label, PrettyPrintType)] -> Maybe PrettyPrintType -> RenderedCode
renderRow h t = renderHead h <> renderTail t

renderHead :: [(Label, PrettyPrintType)] -> RenderedCode
renderHead = mintersperse (syntax "," <> sp) . map renderLabel

renderLabel :: (Label, PrettyPrintType) -> RenderedCode
renderLabel (label, ty) =
  mintersperse sp
    [ typeVar $ prettyPrintLabel label
    , syntax "::"
    , renderType' ty
    ]

renderTail :: Maybe PrettyPrintType -> RenderedCode
renderTail Nothing = mempty
renderTail (Just other) = sp <> syntax "|" <> sp <> renderType' other

typeApp :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
typeApp = mkPattern match
  where
  match (PPTypeApp f x) = Just (f, x)
  match _ = Nothing

kindArg :: Pattern () PrettyPrintType ((), PrettyPrintType)
kindArg = mkPattern match
  where
  match (PPKindArg ty) = Just ((), ty)
  match _ = Nothing

appliedFunction :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
appliedFunction = mkPattern match
  where
  match (PPFunction arg ret) = Just (arg, ret)
  match _ = Nothing

kinded :: Pattern () PrettyPrintType (PrettyPrintType, PrettyPrintType)
kinded = mkPattern match
  where
  match (PPKindedType t k) = Just (t, k)
  match _ = Nothing

constrained :: Pattern () PrettyPrintType (PrettyPrintConstraint, PrettyPrintType)
constrained = mkPattern match
  where
  match (PPConstrainedType con ty) = Just (con, ty)
  match _ = Nothing

explicitParens :: Pattern () PrettyPrintType ((), PrettyPrintType)
explicitParens = mkPattern match
  where
  match (PPParensInType ty) = Just ((), ty)
  match _ = Nothing

matchTypeAtom :: Pattern () PrettyPrintType RenderedCode
matchTypeAtom = typeLiterals <+> fmap parens_ matchType
  where
  parens_ x = syntax "(" <> x <> syntax ")"

matchType :: Pattern () PrettyPrintType RenderedCode
matchType = buildPrettyPrinter operators matchTypeAtom
  where
  operators :: OperatorTable () PrettyPrintType RenderedCode
  operators =
    OperatorTable [ [ Wrap kindArg $ \_ ty -> syntax "@" <> ty ]
                  , [ AssocL typeApp $ \f x -> f <> sp <> x ]
                  , [ AssocR appliedFunction $ \arg ret -> mintersperse sp [arg, syntax "->", ret] ]
                  , [ Wrap constrained $ \deps ty -> renderConstraints deps ty ]
                  , [ Wrap forall_ $ \tyVars ty -> mconcat [ keywordForall, sp, renderTypeVars tyVars, syntax ".", sp, ty ] ]
                  , [ Wrap kinded $ \ty k -> mintersperse sp [renderType' ty, syntax "::", k] ]
                  , [ Wrap explicitParens $ \_ ty -> ty ]
                  ]

forall_ :: Pattern () PrettyPrintType ([(TypeVarVisibility, Text, Maybe PrettyPrintType)], PrettyPrintType)
forall_ = mkPattern match
  where
  match (PPForAll mbKindedIdents ty) = Just (mbKindedIdents, ty)
  match _ = Nothing

renderTypeInternal :: (PrettyPrintType -> PrettyPrintType) -> Type a -> RenderedCode
renderTypeInternal insertRolesIfAny =
  renderType' . insertRolesIfAny . convertPrettyPrintType maxBound

-- |
-- Render code representing a Type
--
renderType :: Type a -> RenderedCode
renderType = renderTypeInternal id

-- |
-- Render code representing a Type
-- but augment the `TypeVar`s with their `Role` if they have one
--
renderTypeWithRole :: [Role] -> Type a -> RenderedCode
renderTypeWithRole = \case
  [] -> renderType
  roleList -> renderTypeInternal (addRole roleList [] . Left)
  where
  -- `data Foo first second = Foo` will produce
  -- ```
  -- PPTypeApp
  --  (PPTypeApp (PPTypeConstructor fooName) (PPTypeVar "first" Nothing))
  --  (PPTypeVar "second" Nothing)
  -- ```
  -- So, we recurse down the left side of `TypeApp` first before
  -- recursing down the right side. To make this stack-safe,
  -- we use a tail-recursive function with its own stack.
  -- - Left = values that have not yet been examined and need
  --          a role added to them (if any). There's still work "left" to do.
  -- - Right = values that have been examined and now need to be
  --           reassembled into their original value
  addRole
    :: [Role]
    -> [Either PrettyPrintType PrettyPrintType]
    -> Either PrettyPrintType PrettyPrintType
    -> PrettyPrintType
  addRole roles stack pp = case pp of
    Left next -> case next of
      PPTypeVar t Nothing
        | Just (x, xs) <- uncons roles ->
          addRole xs stack (Right $ PPTypeVar t (Just $ displayRole x))
        | otherwise ->
          internalError "addRole: invalid arguments - number of roles doesn't match number of type parameters"

      PPTypeVar _ (Just _) ->
        internalError "addRole: attempted to add a second role to a type parameter that already has one"

      PPTypeApp leftSide rightSide -> do
        -- push right-side to stack and continue recursing on left-side
        addRole roles (Left rightSide : stack) (Left leftSide)

      other ->
        -- nothing to check, so move on
        addRole roles stack (Right other)


    pendingAssembly@(Right rightSideOrFinalValue) -> case stack of
      (unfinishedRightSide@(Left _) : remaining) ->
        -- We've finished recursing through the left-side of a `TypeApp`.
        -- Now we'll recurse through the right-side.
        -- We push `pendingAssembly` onto the stack so we can assemble
        -- the `PPTypeApp` together once it's right-side is done.
        addRole roles (pendingAssembly : remaining) unfinishedRightSide

      (Right leftSide : remaining) ->
        -- We've finished recursing through the right-side of a `TypeApp`
        -- We'll rebulid it and wrap it in `Right` so any other higher-level
        -- `TypeApp`s can be reassembled now, too.
        addRole roles remaining (Right (PPTypeApp leftSide rightSideOrFinalValue))

      [] ->
        -- We've reassembled everything. It's time to return.
        rightSideOrFinalValue

renderType' :: PrettyPrintType -> RenderedCode
renderType'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern_ matchType ()

renderTypeVars :: [(TypeVarVisibility, Text, Maybe PrettyPrintType)] -> RenderedCode
renderTypeVars tyVars = mintersperse sp (map renderTypeVar tyVars)

renderTypeVar :: (TypeVarVisibility, Text, Maybe PrettyPrintType) -> RenderedCode
renderTypeVar (vis, v, mbK) = case mbK of
  Nothing -> syntax (typeVarVisibilityPrefix vis) <> typeVar v
  Just k -> mintersperse sp [ mconcat [syntax "(", syntax $ typeVarVisibilityPrefix vis, typeVar v], syntax "::", mconcat [renderType' k, syntax ")"] ]

-- |
-- Render code representing a Type, as it should appear inside parentheses
--
renderTypeAtom :: Type a -> RenderedCode
renderTypeAtom = renderTypeAtom' . convertPrettyPrintType maxBound

renderTypeAtom' :: PrettyPrintType -> RenderedCode
renderTypeAtom'
  = fromMaybe (internalError "Incomplete pattern")
  . PA.pattern_ matchTypeAtom ()
