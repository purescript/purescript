{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.TypeChecker.Synonyms
  ( SynonymMap
  , replaceAllTypeSynonyms
  , replaceAllTypeSynonymsM
  ) where

import           Prelude.Compat

import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.State
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Data.Text (Text)
import           Language.PureScript.Environment
import           Language.PureScript.Errors
import           Language.PureScript.Kinds
import           Language.PureScript.Names
import           Language.PureScript.TypeChecker.Monad
import           Language.PureScript.Types

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe Kind)], Type)

replaceAllTypeSynonyms'
  :: SynonymMap
  -> Type
  -> Either MultipleErrors Type
replaceAllTypeSynonyms' syns = traverseWithoutLooping try
  where
  try :: Type -> Either MultipleErrors Type
  try t = fromMaybe t <$> go 0 [] t

  go :: Int -> [Type] -> Type -> Either MultipleErrors (Maybe Type)
  go c args (TypeConstructor ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args) body
      in Just . ExpandedSynonym (foldl TypeApp (TypeConstructor ctor) args) <$> try repl
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage $ PartiallyAppliedSynonym ctor
  go c args (TypeApp f arg) = go (c + 1) (arg : args) f
  go _ _ _ = return Nothing

  -- This is a variant on 'everywhereOnTypesTopDownM', but which does not recur into
  -- the left hand side of 'ExpandedSynonym', to avoid an infinite loop.
  --
  -- TODO: improve this later
  traverseWithoutLooping :: Monad m => (Type -> m Type) -> Type -> m Type
  traverseWithoutLooping f = go <=< f
    where
    go (TypeApp t1 t2) = TypeApp <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (ForAll arg ty sco) = ForAll arg <$> (f ty >>= go) <*> pure sco
    go (ConstrainedType cs ty) = ConstrainedType <$> mapM (overConstraintArgs (mapM (go <=< f))) cs <*> (f ty >>= go)
    go (RCons name ty rest) = RCons name <$> (f ty >>= go) <*> (f rest >>= go)
    go (KindedType ty k) = KindedType <$> (f ty >>= go) <*> pure k
    go (PrettyPrintFunction t1 t2) = PrettyPrintFunction <$> (f t1 >>= go) <*> (f t2 >>= go)
    go (PrettyPrintObject t) = PrettyPrintObject <$> (f t >>= go)
    go (PrettyPrintForAll args t) = PrettyPrintForAll args <$> (f t >>= go)
    go (BinaryNoParensType t1 t2 t3) = BinaryNoParensType <$> (f t1 >>= go) <*> (f t2 >>= go) <*> (f t3 >>= go)
    go (ParensInType t) = ParensInType <$> (f t >>= go)
    go (ExpandedSynonym t1 t2) = ExpandedSynonym t1 <$> (f t2 >>= go)
    go other = f other

-- | Replace fully applied type synonyms
replaceAllTypeSynonyms :: (e ~ MultipleErrors, MonadState CheckState m, MonadError e m) => Type -> m Type
replaceAllTypeSynonyms d = do
  env <- getEnv
  either throwError return $ replaceAllTypeSynonyms' (typeSynonyms env) d

-- | Replace fully applied type synonyms by explicitly providing a 'SynonymMap'.
replaceAllTypeSynonymsM
  :: MonadError MultipleErrors m
  => SynonymMap
  -> Type
  -> m Type
replaceAllTypeSynonymsM syns = either throwError pure . replaceAllTypeSynonyms' syns
