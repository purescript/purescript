module Language.PureScript.Sugar.LocalTypeSynonyms
  ( desugarLocalTypeSynonymsModule
  ) where

import Prelude.Compat

import Control.Applicative (liftA2)
import Control.Arrow (first, second, (&&&))
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Identity
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Control.Monad.Writer

import Data.Graph (flattenSCC, stronglyConnCompR)
import Data.Foldable (fold, foldl')
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Set as S

import Language.PureScript.AST
import Language.PureScript.Crash
import Language.PureScript.Errors
import Language.PureScript.Names
import Language.PureScript.Traversals
import Language.PureScript.TypeChecker.Synonyms
import Language.PureScript.Types

desugarLocalTypeSynonymsModule :: (MonadError MultipleErrors m, MonadSupply m) => Module -> m Module
desugarLocalTypeSynonymsModule = desugarModule
  where
  desugarModule (Module modSS coms mn decls exps) = Module modSS coms mn <$> prependHoisted (parU decls go) <*> pure exps
  go = go2 . runIdentity . go1
  (go1, _, _, _, _) = everywhereWithContextOnValuesM (UnshadowContext M.empty S.empty) defS unshadowStep defS defS defS
  (go2, _, _, _, _) = everywhereWithContextOnValuesM (HoistContext M.empty M.empty []) defS hoistStepE hoistStepB defS defS

  prependHoisted :: Monad m => WriterT [Declaration] m [Declaration] -> m [Declaration]
  prependHoisted = fmap (uncurry (++)) . runWriterT

-- Step 1: Any type variables that shadow outer variables need to be renamed
-- so the shadowed variables are always available to be given as parameters to
-- the desugared type synonyms.

data UnshadowContext = UnshadowContext { bindings :: M.Map Text Text, usedNames :: S.Set Text }

-- |
-- Adds a name and its unique replacement to an UnshadowContext.
--
updateUnshadowContext :: UnshadowContext -> Text -> Text -> UnshadowContext
updateUnshadowContext UnshadowContext{..} n un =
  UnshadowContext
    { bindings = M.insert n un bindings
    , usedNames = S.insert un usedNames
    }

unshadowStep :: UnshadowContext -> Expr -> Identity (UnshadowContext, Expr)
unshadowStep c = Identity . \case
  (TypedValue check v ty) -> TypedValue check v <$> renameVarsInType c ty
  (Let prov decls expr) -> (c, (Let prov (map (alphaConvertSynonym c) decls) expr))
  other -> (c, other)

renameVarsInType :: UnshadowContext -> SourceType -> (UnshadowContext, SourceType)
renameVarsInType c = \case
  (ForAll ann tv mbK ty sco) ->
    let tv' = if tv `M.member` (bindings c) then genName (`S.member` usedNames c) tv else tv
        (c', ty') = renameVarsInType (updateUnshadowContext c tv tv') ty
    in (c', ForAll ann tv' mbK ty' sco)
  other ->
    let rename t@(TypeVar ann v) = fromMaybe t (TypeVar ann <$> M.lookup v (bindings c))
        rename other' = other'
    in (c, everywhereOnTypes rename other)

alphaConvertSynonym :: UnshadowContext -> Declaration -> Declaration
alphaConvertSynonym c = \case
  (TypeSynonymDeclaration ann name params ty) -> TypeSynonymDeclaration ann name params' ty'
    where
    pnames = map fst params
    pnames' = map (genName (`S.member` (usedNames c))) pnames
    ty' = replaceAllTypeVars (zip pnames (map srcTypeVar pnames')) ty
    params' = zip pnames' (map snd params)
  other -> other

-- Step 2: Hoist all local type synonyms to top-level type synonyms, adding
-- one extra type parameter for each type variable in scope that is used in the
-- synonym. (Adding unused type parameters can cause problems with kind
-- inference, because all unused parameters are assumed to have kind `Type`.)
-- Replace all uses of local type synonyms with uses of the hoisted synonyms.

type SynonymParameter = (Text, Maybe (Type SourceAnn))
type SynonymData = (SourceSpan, [SynonymParameter], SourceType)
type SynonymBindingGroup = M.Map (ProperName 'TypeName) SynonymData

data HoistContext =
  HoistContext
    { synonymMap :: SynonymMap
    , typeVarUseMap :: M.Map (ProperName 'TypeName) (S.Set Text)
    , typeVarsInScope :: [SynonymParameter]
    }

-- |
-- Prepends any quantified type variables into a HoistContext's
-- typeVarsInScope.
--
addTypeVars :: HoistContext -> SourceType -> HoistContext
addTypeVars c t0 = c{ typeVarsInScope = go t0 }
  where
  go (ForAll _ v k t _) = (v, k) : go t
  go _ = typeVarsInScope c

-- |
-- Inserts a new entry into a HoistContext's synonymMap.
--
addSynonym :: Text -> HoistContext -> ProperName 'TypeName -> HoistContext
addSynonym scope c name = c{ synonymMap = M.insert (Qualified Nothing name) ([], hoistedRef) (synonymMap c) }
  where
  args = map srcTypeVar $ filter (`S.member` lookupUsedVars name c) $ map fst $ typeVarsInScope c
  ctor = srcTypeConstructor $ Qualified Nothing $ makeLocalSynonymName scope (length args) name
  hoistedRef = foldr (flip srcTypeApp) ctor args

-- |
-- Updates a HoistContext's typeVarUseMap with the type variables used
-- (transitively) by a collection of local type synonyms.
--
addUsedVars :: HoistContext -> SynonymBindingGroup -> HoistContext
addUsedVars c synGroup = c{ typeVarUseMap = foldl' step typeVarUseMap0 components }
  where
  typeVarUseMap0 = typeVarUseMap c
  findDependencies = filter (liftA2 (||) (`M.member` synGroup) (`M.member` typeVarUseMap0)) . usedTypeNamesFromModule Nothing
  toGraphNode name (_, ps, ty) = (S.fromList $ filter (`notElem` (map fst ps)) $ freeTypeVariables ty, name, findDependencies ty)
  components = map flattenSCC $ stronglyConnCompR $ map (uncurry toGraphNode) $ M.toList synGroup
  step useMap component = foldr (\(_, name, _) -> M.insert name componentVars) useMap component
    where
    componentVars = foldMap (\(vars, _, deps) -> vars <> foldMap (fold . (`M.lookup` useMap)) deps) component
    -- If the component is cyclic, the above lookup may return Nothing; unlike
    -- in lookupUsedVars, this doesn't indicate an internal error. (The user
    -- will get an error about the cyclic type synonym definition later.)

-- |
-- Retrieves the set of type variable names used by a local type synonym from
-- a HoistContext, or crashes if typeVarUseMap hasn't already been updated with
-- the type synonym. (It might be tempting to play it safe and return S.empty,
-- but that would be likely to produce less scrutable errors down the road.)
--
lookupUsedVars :: ProperName 'TypeName -> HoistContext -> S.Set Text
lookupUsedVars k = fromMaybe (internalError "Sugar.LocalTypeSynonyms: name missing from typeVarUseMap") . M.lookup k . typeVarUseMap

hoistStepE :: (MonadError MultipleErrors m, MonadSupply m) => HoistContext -> Expr -> WriterT [Declaration] m (HoistContext, Expr)
hoistStepE c = \case
  (Let prov decls expr) -> do
    scope <- freshName
    let (synGroup, decls') = extractSynonyms decls
        c' = foldl' (addSynonym scope) (addUsedVars c synGroup) (M.keys synGroup)
    M.traverseWithKey (hoistDeclaration scope c') synGroup $> (c', Let prov decls' expr)
  (TypedValue check v ty) ->
    replaceAllTypeSynonymsM (synonymMap c) M.empty ty <&> (addTypeVars c &&& TypedValue check v)
  other -> pure (c, other)

hoistStepB :: (MonadError MultipleErrors m) => HoistContext -> Binder -> WriterT [Declaration] m (HoistContext, Binder)
hoistStepB c = \case
  (TypedBinder ty b) -> replaceAllTypeSynonymsM (synonymMap c) M.empty ty <&> (\ty' -> (c, TypedBinder ty' b))
  other -> pure (c, other)

extractSynonyms :: [Declaration] -> (SynonymBindingGroup, [Declaration])
extractSynonyms = flip foldr mempty $ \case
  (TypeSynonymDeclaration (ss, _) name params ty) -> first $ M.insert name (ss, params, ty)
  other                                           -> second (other :)

hoistDeclaration :: (MonadError MultipleErrors m) => Text -> HoistContext -> ProperName 'TypeName -> SynonymData -> WriterT [Declaration] m ()
hoistDeclaration scope c name (ss, params, ty) = do
  let scopeParams = filter ((`S.member` lookupUsedVars name c) . fst) (typeVarsInScope c)
  let rewriteTypes = replaceAllTypeSynonymsM (synonymMap c) M.empty
  ty' <- rewriteTypes ty
  params' <- traverse (\(pn, pk) -> (pn,) <$> traverse rewriteTypes pk) params
  tell [TypeSynonymDeclaration (ss, []) (makeLocalSynonymName scope (length scopeParams) name) (foldl' (flip (:)) params' scopeParams) ty']
