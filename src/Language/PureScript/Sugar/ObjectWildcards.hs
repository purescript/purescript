module Language.PureScript.Sugar.ObjectWildcards
  ( desugarObjectConstructors
  , desugarDecl
  ) where

import           Prelude.Compat

import           Control.Monad (forM, foldM)
import           Control.Monad.Error.Class (MonadError(..))
import           Control.Monad.Supply.Class
import           Data.List (partition)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Language.PureScript.AST
import           Language.PureScript.Environment (NameKind(..))
import           Language.PureScript.Errors
import           Language.PureScript.Label (Label(..))
import           Language.PureScript.Names
import           Language.PureScript.PSString (PSString)

-- `PathNode` and `PathTree` are used as an intermediate form when desugaring a nested object update.
-- For an update such as:
--
--  x { foo = 0
--    , bar.baz = 1
--    , bar.qux = 2 }
--
-- We represent the updates as the `PathTree`:
--
--  M.fromList [ ("foo", Leaf 3)
--             , ("bar", Branch (M.fromList [ ("baz", Leaf 1)
--                                          , ("qux", Leaf 2) ]) ])
--
-- Which we then convert to an expression representing the following:
--
--   let x' = x
--   in x' { foo = 0
--         , bar = x'.bar { baz = 1
--                        , qux = 2 } }
--
-- The `let` here is required to prevent re-evaluating the object expression `x`.
-- However we don't generate this when using an anonymous argument for the object.
--
type PathTree = Map PSString PathNode
data PathNode = Leaf Expr | Branch PathTree

desugarObjectConstructors
  :: forall m
   . (MonadSupply m, MonadError MultipleErrors m)
  => Module
  -> m Module
desugarObjectConstructors (Module ss coms mn ds exts) = Module ss coms mn <$> mapM desugarDecl ds <*> pure exts

desugarDecl :: forall m. (MonadSupply m, MonadError MultipleErrors m) => Declaration -> m Declaration
desugarDecl (PositionedDeclaration pos com d) = rethrowWithPosition pos $ PositionedDeclaration pos com <$> desugarDecl d
desugarDecl other = fn other
  where
  (fn, _, _) = everywhereOnValuesTopDownM return desugarExpr return

  desugarExpr :: Expr -> m Expr
  desugarExpr AnonymousArgument = throwError . errorMessage $ IncorrectAnonymousArgument
  desugarExpr (Parens b)
    | b' <- stripPositionInfo b
    , BinaryNoParens op val u <- b'
    , isAnonymousArgument u = do arg <- freshIdent'
                                 return $ Abs (Left arg) $ App (App op val) (Var (Qualified Nothing arg))
    | b' <- stripPositionInfo b
    , BinaryNoParens op u val <- b'
    , isAnonymousArgument u = do arg <- freshIdent'
                                 return $ Abs (Left arg) $ App (App op (Var (Qualified Nothing arg))) val
  desugarExpr (Literal (ObjectLiteral ps)) = wrapLambda (Literal . ObjectLiteral) ps
  desugarExpr (ObjectUpdate u ps) | isAnonymousArgument u = do
    obj <- freshIdent'
    Abs (Left obj) <$> wrapLambda (ObjectUpdate (argToExpr obj)) ps
  desugarExpr (ObjectUpdate obj ps) = wrapLambda (ObjectUpdate obj) ps
  desugarExpr (ObjectUpdateNested obj ps) = transformNestedUpdate obj ps
  desugarExpr (Accessor prop u)
    | Just props <- peelAnonAccessorChain u = do
      arg <- freshIdent'
      return $ Abs (Left arg) $ foldr Accessor (argToExpr arg) (prop:props)
  desugarExpr (Case args cas) | any isAnonymousArgument args = do
    argIdents <- forM args freshIfAnon
    let args' = zipWith (`maybe` argToExpr) args argIdents
    return $ foldr (Abs . Left) (Case args' cas) (catMaybes argIdents)
  desugarExpr (IfThenElse u t f) | any isAnonymousArgument [u, t, f] = do
    u' <- freshIfAnon u
    t' <- freshIfAnon t
    f' <- freshIfAnon f
    let if_ = IfThenElse (maybe u argToExpr u') (maybe t argToExpr t') (maybe f argToExpr f')
    return $ foldr (Abs . Left) if_ (catMaybes [u', t', f'])
  desugarExpr e = return e

  transformNestedUpdate :: Expr -> [(NonEmpty PSString, Expr)] -> m Expr
  transformNestedUpdate obj ps = do
    -- If we don't have an anonymous argument then we need to generate a let wrapper
    -- so that the object expression isn't re-evaluated for each nested update.
    val <- freshIdent'
    if isAnonymousArgument obj
      then Abs (Left val) <$> wrapLambdaM (build val) ps
      else wrapLambdaM (fmap (buildLet val) . build val) ps
    where
      build val xs = buildUpdates (argToExpr val) <$> foldM buildTree M.empty xs
      buildLet val = Let [ValueDeclaration val Public [] (Right obj)]

      -- Here we build up the intermediate `PathTree` data structure and check that
      -- the paths are valid relative to each other - for example, the update
      -- `{ foo.bar = 2, foo = {bar: 3} }` is invalid because there are conflicting
      -- paths.
      buildTree
        :: PathTree
        -> (NonEmpty PSString, Expr)
        -> m PathTree
      buildTree pathTree (path, e) = go pathTree path where
        go tree (key :| [])
          -- path already exists
          | key `M.member` tree = throwError . errorMessage $ DuplicateLabel (Label key) (Just (ObjectUpdateNested obj ps))
          -- create new path
          | otherwise         = return (M.insert key (Leaf e) tree)
        go tree (key :| (x : xs)) = do
          branch <- case M.lookup key tree of
            -- nothing at this path yet
            Nothing -> return M.empty
            -- already a map at this path
            Just (Branch branch) -> return branch
            -- sub-path already exists
            Just (Leaf _) -> throwError . errorMessage $ DuplicateLabel (Label key) (Just (ObjectUpdateNested obj ps))
          M.insert key . Branch <$> go branch (x :| xs) <*> pure tree

      -- Now we have a valid collection of updates in the form of a `PathTree`
      -- we can recursively build up the nested `ObjectUpdate` expressions.
      buildUpdates :: Expr -> PathTree -> Expr
      buildUpdates val vs = ObjectUpdate val (goLayer [] <$> M.toList vs) where
        goLayer :: [PSString] -> (PSString, PathNode) -> (PSString, Expr)
        goLayer _ (key, Leaf expr) = (key, expr)
        goLayer path (key, Branch branch) =
          let path' = path ++ [key]
              updates = goLayer path' <$> M.toList branch
              accessor = foldr Accessor val path'
              objectUpdate = ObjectUpdate accessor updates
          in (key, objectUpdate)

  wrapLambdaM :: forall k. ([(k, Expr)] -> m Expr) -> [(k, Expr)] -> m Expr
  wrapLambdaM mkVal ps =
    let (args, props) = partition (isAnonymousArgument . snd) ps
    in if null args
       then mkVal props
       else do
        (args', ps') <- unzip <$> mapM mkProp ps
        val <- mkVal ps'
        return $ foldr (Abs . Left) val (catMaybes args')

  wrapLambda :: forall k. ([(k, Expr)] -> Expr) -> [(k, Expr)] -> m Expr
  wrapLambda mkVal = wrapLambdaM (return . mkVal)

  stripPositionInfo :: Expr -> Expr
  stripPositionInfo (PositionedValue _ _ e) = stripPositionInfo e
  stripPositionInfo e = e

  peelAnonAccessorChain :: Expr -> Maybe [PSString]
  peelAnonAccessorChain (Accessor p e) = (p :) <$> peelAnonAccessorChain e
  peelAnonAccessorChain (PositionedValue _ _ e) = peelAnonAccessorChain e
  peelAnonAccessorChain AnonymousArgument = Just []
  peelAnonAccessorChain _ = Nothing

  isAnonymousArgument :: Expr -> Bool
  isAnonymousArgument AnonymousArgument = True
  isAnonymousArgument (PositionedValue _ _ e) = isAnonymousArgument e
  isAnonymousArgument _ = False

  mkProp :: forall k. (k, Expr) -> m (Maybe Ident, (k, Expr))
  mkProp (name, e) = do
    arg <- freshIfAnon e
    return (arg, (name, maybe e argToExpr arg))

  freshIfAnon :: Expr -> m (Maybe Ident)
  freshIfAnon u
    | isAnonymousArgument u = Just <$> freshIdent'
    | otherwise = return Nothing

  argToExpr :: Ident -> Expr
  argToExpr = Var . Qualified Nothing
