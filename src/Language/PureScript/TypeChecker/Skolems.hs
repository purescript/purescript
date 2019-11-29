-- | Functions relating to skolemization used during typechecking
module Language.PureScript.TypeChecker.Skolems
  ( newSkolemConstant
  , introduceSkolemScope
  , newSkolemScope
  , skolemize
  , skolemizeTypesInValue
  , skolemEscapeCheck
  ) where

import Prelude.Compat

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(), runIdentity)
import Data.Set (Set, fromList, notMember)
import Data.Text (Text)
import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Traversals (defS)
import Language.PureScript.TypeChecker.Monad
import Language.PureScript.Types

-- | Generate a new skolem constant
newSkolemConstant :: MonadState CheckState m => m Int
newSkolemConstant = do
  s <- gets checkNextSkolem
  modify $ \st -> st { checkNextSkolem = s + 1 }
  return s

-- | Introduce skolem scope at every occurrence of a ForAll
introduceSkolemScope :: MonadState CheckState m => Type a -> m (Type a)
introduceSkolemScope = everywhereOnTypesM go
  where
  go (ForAll ann ident mbK ty Nothing) = ForAll ann ident mbK ty <$> (Just <$> newSkolemScope)
  go other = return other

-- | Generate a new skolem scope
newSkolemScope :: MonadState CheckState m => m SkolemScope
newSkolemScope = do
  s <- gets checkNextSkolemScope
  modify $ \st -> st { checkNextSkolemScope = s + 1 }
  return $ SkolemScope s

-- | Skolemize a type variable by replacing its instances with fresh skolem constants
skolemize :: a -> Text -> Int -> SkolemScope -> Type a -> Type a
skolemize ann ident sko scope = replaceTypeVars ident (Skolem ann ident sko scope)

-- | This function skolemizes type variables appearing in any type signatures or
-- 'DeferredDictionary' placeholders. These type variables are the only places
-- where scoped type variables can appear in expressions.
skolemizeTypesInValue :: SourceAnn -> Text -> Int -> SkolemScope -> Expr -> Expr
skolemizeTypesInValue ann ident sko scope =
    runIdentity . onExpr'
  where
    onExpr' :: Expr -> Identity Expr
    (_, onExpr', _, _, _) = everywhereWithContextOnValuesM [] defS onExpr onBinder defS defS

    onExpr :: [Text] -> Expr -> Identity ([Text], Expr)
    onExpr sco (DeferredDictionary c ts)
      | ident `notElem` sco = return (sco, DeferredDictionary c (map (skolemize ann ident sko scope) ts))
    onExpr sco (TypedValue check val ty)
      | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedValue check val (skolemize ann ident sko scope ty))
    onExpr sco other = return (sco, other)

    onBinder :: [Text] -> Binder -> Identity ([Text], Binder)
    onBinder sco (TypedBinder ty b)
      | ident `notElem` sco = return (sco ++ peelTypeVars ty, TypedBinder (skolemize ann ident sko scope ty) b)
    onBinder sco other = return (sco, other)

    peelTypeVars :: SourceType -> [Text]
    peelTypeVars (ForAll _ i _ ty _) = i : peelTypeVars ty
    peelTypeVars _ = []

-- | Ensure skolem variables do not escape their scope
--
-- Every skolem variable is created when a 'ForAll' type is skolemized.
-- This determines the scope of that skolem variable, which is copied from
-- the 'SkolemScope' field of the 'ForAll' constructor.
--
-- This function traverses the tree top-down, and collects any 'SkolemScope's
-- introduced by 'ForAll's. If a 'Skolem' is encountered whose 'SkolemScope' is
-- not in the current list, then we have found an escaped skolem variable.
skolemEscapeCheck :: MonadError MultipleErrors m => Expr -> m ()
skolemEscapeCheck (TypedValue False _ _) = return ()
skolemEscapeCheck expr@TypedValue{} =
    traverse_ (throwError . singleError) (toSkolemErrors expr)
  where
    toSkolemErrors :: Expr -> [ErrorMessage]
    (_, toSkolemErrors, _, _, _) = everythingWithContextOnValues (mempty, Nothing) [] (<>) def go def def def

    def s _ = (s, [])

    go :: (Set SkolemScope, Maybe SourceSpan)
       -> Expr
       -> ((Set SkolemScope, Maybe SourceSpan), [ErrorMessage])
    go (scopes, _) (PositionedValue ss _ _) = ((scopes, Just ss), [])
    go (scopes, ssUsed) val@(TypedValue _ _ ty) =
        ( (allScopes, ssUsed)
        , [ ErrorMessage (maybe id ((:) . positionedError) ssUsed [ ErrorInExpression val ]) $
              EscapedSkolem name (nonEmptySpan ssBound) ty
          | (ssBound, name, scope) <- collectSkolems ty
          , notMember scope allScopes
          ]
        )
      where
        -- Any new skolem scopes introduced by universal quantifiers
        newScopes :: [SkolemScope]
        newScopes = collectScopes ty

        -- All scopes, including new scopes
        allScopes :: Set SkolemScope
        allScopes = fromList newScopes <> scopes

        -- Collect any scopes appearing in quantifiers at the top level
        collectScopes :: SourceType -> [SkolemScope]
        collectScopes (ForAll _ _ _ t (Just sco)) = sco : collectScopes t
        collectScopes ForAll{} = internalError "skolemEscapeCheck: No skolem scope"
        collectScopes _ = []

        -- Collect any skolem variables appearing in a type
        collectSkolems :: SourceType -> [(SourceAnn, Text, SkolemScope)]
        collectSkolems = everythingOnTypes (++) collect where
          collect (Skolem ss name _ scope) = [(ss, name, scope)]
          collect _ = []
    go scos _ = (scos, [])
skolemEscapeCheck _ = internalError "skolemEscapeCheck: untyped value"
