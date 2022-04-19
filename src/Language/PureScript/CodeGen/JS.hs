-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Applicative (liftA2)
import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class
import Control.Monad.Writer (MonadWriter, runWriterT, writer)

import Data.Bifunctor (first)
import Data.List ((\\), intersect)
import qualified Data.List.NonEmpty as NEL (nonEmpty)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Monoid (Any(..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreImp.AST (AST, everywhere, everywhereTopDownM, withSourceSpan)
import qualified Language.PureScript.CoreImp.AST as AST
import qualified Language.PureScript.CoreImp.Module as AST
import Language.PureScript.CoreImp.Optimizer
import Language.PureScript.CoreFn
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants.Prim as C

import System.FilePath.Posix ((</>))

-- | Generate code in the simplified JavaScript intermediate representation for all declarations in a
-- module.
moduleToJs
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe PSString
  -> m AST.Module
moduleToJs (Module _ coms mn _ imps exps reExps foreigns decls) foreignInclude =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    let decls' = renameModules mnLookup decls
    (jsDecls, Any needRuntimeLazy) <- runWriterT $ mapM (moduleBindToJs mn) decls'
    let mnReverseLookup = M.fromList $ map (\(origName, (_, safeName)) -> (moduleNameToJs safeName, origName)) $ M.toList mnLookup
    let moduleObjectNames = "$foreign" `S.insert` M.keysSet mnReverseLookup
    optimized <- traverse (traverse (fmap (annotatePure moduleObjectNames) . optimize)) (if needRuntimeLazy then [runtimeLazy] : jsDecls else jsDecls)
    let usedModuleNames = foldMap (foldMap (findModules mnReverseLookup)) optimized
          `S.union` M.keysSet reExps
    let jsImports
          = map (importToJs mnLookup)
          . filter (flip S.member usedModuleNames)
          . (\\ (mn : C.primModules)) $ ordNub $ map snd imps
    F.traverse_ (F.traverse_ checkIntegers) optimized
    comments <- asks (not . optionsNoComments)
    let header = if comments then coms else []
    let foreign' = maybe [] (pure . AST.Import Nothing "$foreign") $ if null foreigns then Nothing else foreignInclude
    let moduleBody = concat optimized
    let foreignExps = exps `intersect` foreigns
    let standardExps = exps \\ foreignExps
    let reExps' = M.toList (M.withoutKeys reExps (S.fromList C.primModules))
    let jsExports
          =  (maybeToList . exportsToJs foreignInclude $ foreignExps)
          ++ (maybeToList . exportsToJs Nothing $ standardExps)
          ++  mapMaybe reExportsToJs reExps'
    return $ AST.Module header (foreign' ++ jsImports) moduleBody jsExports

  where
  -- | Adds purity annotations to top-level values for bundlers.
  -- The semantics here derive from treating top-level module evaluation as pure, which lets
  -- us remove any unreferenced top-level declarations. To achieve this, we wrap any non-trivial
  -- top-level values in an IIFE marked with a pure annotation.
  annotatePure :: S.Set Text -> AST -> AST
  annotatePure moduleObjectNames = annotateOrWrap
    where
    annotateOrWrap = liftA2 fromMaybe pureIife maybePure

    -- | If the JS is potentially effectful (in the eyes of a bundler that
    -- doesn't know about PureScript), return Nothing. Otherwise, return Just
    -- the JS with any needed pure annotations added, and, in the case of a
    -- variable declaration, an IIFE to be annotated.
    maybePure :: AST -> Maybe AST
    maybePure = maybePureGen False

    -- | Like maybePure, but doesn't add a pure annotation to App. This exists
    -- to prevent from doubling up on annotation comments on curried
    -- applications; from experimentation, it turns out that a comment on the
    -- outermost App is sufficient for the entire curried chain to be
    -- considered effect-free.
    maybePure' :: AST -> Maybe AST
    maybePure' = maybePureGen True

    maybePureGen alreadyAnnotated = \case
      AST.VariableIntroduction ss name j -> Just (AST.VariableIntroduction ss name (annotateOrWrap <$> j))
      AST.App ss f args -> (if alreadyAnnotated then AST.App else pureApp) ss <$> maybePure' f <*> traverse maybePure args
      -- In general, indexers can be effectful, but not when indexing into an
      -- ES module object.
      AST.Indexer ss idx v@(AST.Var _ name)
        | name `S.member` moduleObjectNames -> (\idx' -> AST.Indexer ss idx' v) <$> maybePure idx
      AST.ArrayLiteral ss jss -> AST.ArrayLiteral ss <$> traverse maybePure jss
      AST.ObjectLiteral ss props -> AST.ObjectLiteral ss <$> traverse (traverse maybePure) props
      AST.Comment c js -> AST.Comment c <$> maybePure js

      js@AST.NumericLiteral{} -> Just js
      js@AST.StringLiteral{}  -> Just js
      js@AST.BooleanLiteral{} -> Just js
      js@AST.Function{}       -> Just js
      js@AST.Var{}            -> Just js

      _ -> Nothing

    pureIife :: AST -> AST
    pureIife val = pureApp Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing [AST.Return Nothing val])) []

    pureApp :: Maybe SourceSpan -> AST -> [AST] -> AST
    pureApp ss f = AST.Comment AST.PureAnnotation . AST.App ss f

  -- | Extracts all declaration names from a binding group.
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- | Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
  renameImports = go M.empty
    where
    go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
    go acc used ((ann, mn') : mns') =
      let mni = Ident $ moduleNameToJs mn'
      in if mn' /= mn && mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' (ann, mn') acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName name) used =
      let newName = ModuleName $ name <> "_" <> T.pack (show i)
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- | Generates JavaScript code for a module import, binding the required module
  -- to the alternative
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> AST.Import
  importToJs mnLookup mn' =
    let ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    in AST.Import (Just ss) (moduleNameToJs mnSafe) (moduleImportPath mn')

  -- | Generates JavaScript code for exporting at least one identifier,
  -- eventually from another module.
  exportsToJs :: Maybe PSString -> [Ident] -> Maybe AST.Export
  exportsToJs from = fmap (flip AST.Export from) . NEL.nonEmpty . fmap runIdent

  -- | Generates JavaScript code for re-exporting at least one identifier from
  -- from another module.
  reExportsToJs :: (ModuleName, [Ident]) -> Maybe AST.Export
  reExportsToJs = uncurry exportsToJs . first (Just . moduleImportPath)

  moduleImportPath :: ModuleName -> PSString
  moduleImportPath mn' = fromString (".." </> T.unpack (runModuleName mn') </> "index.js")

  -- | Replaces the `ModuleName`s in the AST so that the generated code refers to
  -- the collision-avoiding renamed module imports.
  renameModules :: M.Map ModuleName (Ann, ModuleName) -> [Bind Ann] -> [Bind Ann]
  renameModules mnLookup binds =
    let (f, _, _) = everywhereOnValues id goExpr goBinder
    in map f binds
    where
    goExpr :: Expr a -> Expr a
    goExpr (Var ann q) = Var ann (renameQual q)
    goExpr e = e
    goBinder :: Binder a -> Binder a
    goBinder (ConstructorBinder ann q1 q2 bs) = ConstructorBinder ann (renameQual q1) (renameQual q2) bs
    goBinder b = b
    renameQual :: Qualified a -> Qualified a
    renameQual (Qualified (Just mn') a) =
      let (_,mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in Qualified (Just mnSafe) a
    renameQual q = q

  -- |
  -- Find the set of ModuleNames referenced by an AST.
  --
  findModules :: M.Map Text ModuleName -> AST -> S.Set ModuleName
  findModules mnReverseLookup = AST.everything mappend go
    where
    go (AST.Var _ name) = foldMap S.singleton $ M.lookup name mnReverseLookup
    go _ = mempty

  -- Check that all integers fall within the valid int range for JavaScript.
  checkIntegers :: AST -> m ()
  checkIntegers = void . everywhereTopDownM go
    where
    go :: AST -> m AST
    go (AST.Unary _ AST.Negate (AST.NumericLiteral ss (Left i))) =
      -- Move the negation inside the literal; since this is a top-down
      -- traversal doing this replacement will stop the next case from raising
      -- the error when attempting to use -2147483648, as if left unrewritten
      -- the value is `Unary Negate (NumericLiteral (Left 2147483648))`, and
      -- 2147483648 is larger than the maximum allowed int.
      return $ AST.NumericLiteral ss (Left (-i))
    go js@(AST.NumericLiteral ss (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError . maybe errorMessage errorMessage' ss $ IntOutOfRange i "JavaScript" minInt maxInt
         else return js
    go other = return other

  runtimeLazy :: AST
  runtimeLazy =
    AST.VariableIntroduction Nothing "$runtime_lazy" . Just . AST.Function Nothing Nothing ["name", "moduleName", "init"] . AST.Block Nothing $
      [ AST.VariableIntroduction Nothing "state" . Just . AST.NumericLiteral Nothing $ Left 0
      , AST.VariableIntroduction Nothing "val" Nothing
      , AST.Return Nothing . AST.Function Nothing Nothing ["lineNumber"] . AST.Block Nothing $
        [ AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing "state") (AST.NumericLiteral Nothing (Left 2))) (AST.Return Nothing $ AST.Var Nothing "val") Nothing
        , AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing "state") (AST.NumericLiteral Nothing (Left 1))) (AST.Throw Nothing $ AST.Unary Nothing AST.New (AST.App Nothing (AST.Var Nothing "ReferenceError") [foldl1 (AST.Binary Nothing AST.Add)
          [ AST.Var Nothing "name"
          , AST.StringLiteral Nothing " was needed before it finished initializing (module "
          , AST.Var Nothing "moduleName"
          , AST.StringLiteral Nothing ", line "
          , AST.Var Nothing "lineNumber"
          , AST.StringLiteral Nothing ")"
          ], AST.Var Nothing "moduleName", AST.Var Nothing "lineNumber"])) Nothing
        , AST.Assignment Nothing (AST.Var Nothing "state") . AST.NumericLiteral Nothing $ Left 1
        , AST.Assignment Nothing (AST.Var Nothing "val") $ AST.App Nothing (AST.Var Nothing "init") []
        , AST.Assignment Nothing (AST.Var Nothing "state") . AST.NumericLiteral Nothing $ Left 2
        , AST.Return Nothing $ AST.Var Nothing "val"
        ]
      ]


moduleBindToJs
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadWriter Any m, MonadError MultipleErrors m)
  => ModuleName
  -> Bind Ann
  -> m [AST]
moduleBindToJs mn = bindToJs
  where
  -- |
  -- Generate code in the simplified JavaScript intermediate representation for a declaration
  --
  bindToJs :: Bind Ann -> m [AST]
  bindToJs (NonRec (_, _, _, Just IsTypeClassConstructor) _ _) = pure []
    -- Unlike other newtype constructors, type class constructors are only
    -- ever applied; it's not possible to use them as values. So it's safe to
    -- erase them.
  bindToJs (NonRec ann ident val) = return <$> nonRecToJS ann ident val
  bindToJs (Rec vals) = writer (applyLazinessTransform mn vals) >>= traverse (uncurry . uncurry $ nonRecToJS)

  -- | Generate code in the simplified JavaScript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  nonRecToJS :: Ann -> Ident -> Expr Ann -> m AST
  nonRecToJS a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS a i (modifyAnn removeComments e)
       else AST.Comment (AST.SourceComments com) <$> nonRecToJS a i (modifyAnn removeComments e)
  nonRecToJS (ss, _, _, _) ident val = do
    js <- valueToJs val
    return $ AST.VariableIntroduction (Just ss) (identToJs ident) (Just js)

  -- | Generate code in the simplified JavaScript intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToJs

  var' :: SourceSpan -> Ident -> AST
  var' ss = withSourceSpan ss . var

  -- | Generate code in the simplified JavaScript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in JavaScript (symbol based, reserved name) an
  -- indexer is returned.
  moduleAccessor :: Ident -> AST -> AST
  moduleAccessor (Ident prop) = moduleAccessorString prop
  moduleAccessor (GenIdent _ _) = internalError "GenIdent in moduleAccessor"
  moduleAccessor UnusedIdent = internalError "UnusedIdent in moduleAccessor"
  moduleAccessor InternalIdent{} = internalError "InternalIdent in moduleAccessor"

  moduleAccessorString :: Text -> AST -> AST
  moduleAccessorString = accessorString . mkString . T.replace "'" "$prime"

  accessorString :: PSString -> AST -> AST
  accessorString prop = AST.Indexer Nothing (AST.StringLiteral Nothing prop)

  -- | Generate code in the simplified JavaScript intermediate representation for a value or expression.
  valueToJs :: Expr Ann -> m AST
  valueToJs (Literal (pos, _, _, _) l) =
    rethrowWithPosition pos $ literalToValueJS pos l
  valueToJs (Var (ss, _, _, Just (IsConstructor _ [])) name) =
    return $ accessorString "value" $ qualifiedToJS ss id name
  valueToJs (Var (ss, _, _, Just (IsConstructor _ _)) name) =
    return $ accessorString "create" $ qualifiedToJS ss id name
  valueToJs (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs (ObjectUpdate (ss, _, _, _) o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj ss obj sts
  valueToJs (Abs _ arg val) = do
    ret <- valueToJs val
    let jsArg = case arg of
                  UnusedIdent -> []
                  _           -> [identToJs arg]
    return $ AST.Function Nothing Nothing jsArg (AST.Block Nothing [AST.Return Nothing ret])
  valueToJs e@(App (ss, _, _, _) _ _) = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (ss', _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ AST.Unary Nothing AST.New $ AST.App (Just ss) (qualifiedToJS ss' id name) args'
      _ -> flip (foldl (\fn a -> AST.App (Just ss) fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs (Var (ss, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs ss qi
  valueToJs (Var (_, _, _, Just IsForeign) ident) =
    internalError $ "Encountered an unqualified reference to a foreign ident " ++ T.unpack (showQualified showIdent ident)
  valueToJs (Var (ss, _, _, _) ident) = return $ varToJs ss ident
  valueToJs (Case (ss, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs ss binders vals
  valueToJs (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (ds' ++ [AST.Return Nothing ret]))) []
  valueToJs (Constructor (_, _, _, Just IsNewtype) _ ctor _) =
    return $ AST.VariableIntroduction Nothing (properToJs ctor) (Just $
                AST.ObjectLiteral Nothing [("create",
                  AST.Function Nothing Nothing ["value"]
                    (AST.Block Nothing [AST.Return Nothing $ AST.Var Nothing "value"]))])
  valueToJs (Constructor _ _ ctor []) =
    return $ iife (properToJs ctor) [ AST.Function Nothing (Just (properToJs ctor)) [] (AST.Block Nothing [])
           , AST.Assignment Nothing (accessorString "value" (AST.Var Nothing (properToJs ctor)))
                (AST.Unary Nothing AST.New $ AST.App Nothing (AST.Var Nothing (properToJs ctor)) []) ]
  valueToJs (Constructor _ _ ctor fields) =
    let constructor =
          let body = [ AST.Assignment Nothing ((accessorString $ mkString $ identToJs f) (AST.Var Nothing "this")) (var f) | f <- fields ]
          in AST.Function Nothing (Just (properToJs ctor)) (identToJs `map` fields) (AST.Block Nothing body)
        createFn =
          let body = AST.Unary Nothing AST.New $ AST.App Nothing (AST.Var Nothing (properToJs ctor)) (var `map` fields)
          in foldr (\f inner -> AST.Function Nothing Nothing [identToJs f] (AST.Block Nothing [AST.Return Nothing inner])) body fields
    in return $ iife (properToJs ctor) [ constructor
                          , AST.Assignment Nothing (accessorString "create" (AST.Var Nothing (properToJs ctor))) createFn
                          ]

  iife :: Text -> [AST] -> AST
  iife v exprs = AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing $ exprs ++ [AST.Return Nothing $ AST.Var Nothing v])) []

  literalToValueJS :: SourceSpan -> Literal (Expr Ann) -> m AST
  literalToValueJS ss (NumericLiteral (Left i)) = return $ AST.NumericLiteral (Just ss) (Left i)
  literalToValueJS ss (NumericLiteral (Right n)) = return $ AST.NumericLiteral (Just ss) (Right n)
  literalToValueJS ss (StringLiteral s) = return $ AST.StringLiteral (Just ss) s
  literalToValueJS ss (CharLiteral c) = return $ AST.StringLiteral (Just ss) (fromString [c])
  literalToValueJS ss (BooleanLiteral b) = return $ AST.BooleanLiteral (Just ss) b
  literalToValueJS ss (ArrayLiteral xs) = AST.ArrayLiteral (Just ss) <$> mapM valueToJs xs
  literalToValueJS ss (ObjectLiteral ps) = AST.ObjectLiteral (Just ss) <$> mapM (sndM valueToJs) ps

  -- | Shallow copy an object.
  extendObj :: SourceSpan -> AST -> [(PSString, AST)] -> m AST
  extendObj ss obj sts = do
    newObj <- freshName
    key <- freshName
    evaluatedObj <- freshName
    let
      jsKey = AST.Var Nothing key
      jsNewObj = AST.Var Nothing newObj
      jsEvaluatedObj = AST.Var Nothing evaluatedObj
      block = AST.Block Nothing (evaluate:objAssign:copy:extend ++ [AST.Return Nothing jsNewObj])
      evaluate = AST.VariableIntroduction Nothing evaluatedObj (Just obj)
      objAssign = AST.VariableIntroduction (Just ss) newObj (Just $ AST.ObjectLiteral Nothing [])
      copy = AST.ForIn Nothing key jsEvaluatedObj $ AST.Block Nothing [AST.IfElse Nothing cond assign Nothing]
      cond = AST.App Nothing (accessorString "call" (accessorString "hasOwnProperty" (AST.ObjectLiteral Nothing []))) [jsEvaluatedObj, jsKey]
      assign = AST.Block Nothing [AST.Assignment Nothing (AST.Indexer Nothing jsKey jsNewObj) (AST.Indexer Nothing jsKey jsEvaluatedObj)]
      stToAssign (s, js) = AST.Assignment Nothing (accessorString s jsNewObj) js
      extend = map stToAssign sts
    return $ AST.App Nothing (AST.Function Nothing Nothing [] block) []

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable.
  varToJs :: SourceSpan -> Qualified Ident -> AST
  varToJs ss (Qualified Nothing ident) = var' ss ident
  varToJs ss qual = qualifiedToJS ss id qual

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToJS :: SourceSpan -> (a -> Ident) -> Qualified a -> AST
  qualifiedToJS _ f (Qualified (Just C.Prim) a) = AST.Var Nothing . runIdent $ f a
  qualifiedToJS ss f (Qualified (Just mn') a) | mn /= mn' = moduleAccessor (f a) (AST.Var (Just ss) (moduleNameToJs mn'))
  qualifiedToJS ss f (Qualified _ a) = AST.Var (Just ss) $ identToJs (f a)

  foreignIdent :: Ident -> AST
  foreignIdent ident = accessorString (mkString $ runIdent ident) (AST.Var Nothing "$foreign")

  -- | Generate code in the simplified JavaScript intermediate representation for pattern match binders
  -- and guards.
  bindersToJs :: SourceSpan -> [CaseAlternative Ann] -> [AST] -> m AST
  bindersToJs ss binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (AST.VariableIntroduction Nothing) valNames (map Just vals)
    jss <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToJs result
      go valNames ret bs
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (assignments ++ concat jss ++ [AST.Throw Nothing $ failedPatternError valNames])))
                   []
    where
      go :: [Text] -> [AST] -> [Binder Ann] -> m [AST]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToJs v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToJs"

      failedPatternError :: [Text] -> AST
      failedPatternError names = AST.Unary Nothing AST.New $ AST.App Nothing (AST.Var Nothing "Error") [AST.Binary Nothing AST.Add (AST.StringLiteral Nothing $ mkString failedPatternMessage) (AST.ArrayLiteral Nothing $ zipWith valueError names vals)]

      failedPatternMessage :: Text
      failedPatternMessage = "Failed pattern match at " <> runModuleName mn <> " " <> displayStartEndPos ss <> ": "

      valueError :: Text -> AST -> AST
      valueError _ l@(AST.NumericLiteral _ _) = l
      valueError _ l@(AST.StringLiteral _ _)  = l
      valueError _ l@(AST.BooleanLiteral _ _) = l
      valueError s _                        = accessorString "name" . accessorString "constructor" $ AST.Var Nothing s

      guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [AST]
      guardsToJs (Left gs) = traverse genGuard gs where
        genGuard (cond, val) = do
          cond' <- valueToJs cond
          val'   <- valueToJs val
          return
            (AST.IfElse Nothing cond'
              (AST.Block Nothing [AST.Return Nothing val']) Nothing)

      guardsToJs (Right v) = return . AST.Return Nothing <$> valueToJs v

  -- | Generate code in the simplified JavaScript intermediate representation for a pattern match
  -- binder.
  binderToJs :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs _ done NullBinder{} = return done
  binderToJs varName done (LiteralBinder (ss, _, _, _) l) =
    literalToBinderJS ss varName done l
  binderToJs varName done (VarBinder (ss, _, _, _) ident) =
    return (AST.VariableIntroduction (Just ss) (identToJs ident) (Just (AST.Var Nothing varName)) : done)
  binderToJs varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs varName done (ConstructorBinder (ss, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    js <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> js
      SumType ->
        [AST.IfElse Nothing (AST.InstanceOf Nothing (AST.Var Nothing varName) (qualifiedToJS ss (Ident . runProperName) ctor))
                  (AST.Block Nothing js)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [AST] -> m [AST]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      js <- binderToJs argVar done'' binder
      return (AST.VariableIntroduction Nothing argVar (Just $ accessorString (mkString $ identToJs field) $ AST.Var Nothing varName) : js)
  binderToJs _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (AST.Var Nothing varName)) : js)

  literalToBinderJS :: SourceSpan -> Text -> [AST] -> Literal (Binder Ann) -> m [AST]
  literalToBinderJS ss varName done (NumericLiteral num) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.NumericLiteral (Just ss) num)) (AST.Block Nothing done) Nothing]
  literalToBinderJS ss varName done (CharLiteral c) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral (Just ss) (fromString [c]))) (AST.Block Nothing done) Nothing]
  literalToBinderJS ss varName done (StringLiteral str) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral (Just ss) str)) (AST.Block Nothing done) Nothing]
  literalToBinderJS ss varName done (BooleanLiteral True) =
    return [AST.IfElse Nothing (AST.Var (Just ss) varName) (AST.Block Nothing done) Nothing]
  literalToBinderJS ss varName done (BooleanLiteral False) =
    return [AST.IfElse Nothing (AST.Unary Nothing AST.Not (AST.Var (Just ss) varName)) (AST.Block Nothing done) Nothing]
  literalToBinderJS _ varName done (ObjectLiteral bs) = go done bs
    where
    go :: [AST] -> [(PSString, Binder Ann)] -> m [AST]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      js <- binderToJs propVar done'' binder
      return (AST.VariableIntroduction Nothing propVar (Just (accessorString prop (AST.Var Nothing varName))) : js)
  literalToBinderJS _ varName done (ArrayLiteral bs) = do
    js <- go done 0 bs
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (accessorString "length" (AST.Var Nothing varName)) (AST.NumericLiteral Nothing (Left (fromIntegral $ length bs)))) (AST.Block Nothing js) Nothing]
    where
    go :: [AST] -> Integer -> [Binder Ann] -> m [AST]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      js <- binderToJs elVar done'' binder
      return (AST.VariableIntroduction Nothing elVar (Just (AST.Indexer Nothing (AST.NumericLiteral Nothing (Left index)) (AST.Var Nothing varName))) : js)
