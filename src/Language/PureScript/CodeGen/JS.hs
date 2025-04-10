-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  ) where

import Prelude
import Protolude (ordNub)

import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class (MonadSupply, freshName)
import Control.Monad.Writer (MonadWriter, runWriterT, writer)

import Data.Bifunctor (first)
import Data.List ((\\), intersect)
import Data.List.NonEmpty qualified as NEL (nonEmpty)
import Data.Foldable qualified as F
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Monoid (Any(..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T

import Language.PureScript.AST.SourcePos (SourceSpan, displayStartEndPos)
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreImp.AST (AST, InitializerEffects(..), everywhere, everywhereTopDownM, withSourceSpan)
import Language.PureScript.CoreImp.AST qualified as AST
import Language.PureScript.CoreImp.Module qualified as AST
import Language.PureScript.CoreImp.Optimizer (optimize)
import Language.PureScript.CoreFn (Ann, Bind(..), Binder(..), CaseAlternative(..), ConstructorType(..), Expr(..), Guard, Literal(..), Meta(..), Module(..), extractAnn, extractBinderAnn, modifyAnn, removeComments)
import Language.PureScript.CoreFn.Laziness (applyLazinessTransform)
import Language.PureScript.Crash (internalError)
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow, errorMessage,
                                   errorMessage', rethrowWithPosition, addHint)
import Language.PureScript.Names (Ident(..), ModuleName, ProperName(..), Qualified(..), QualifiedBy(..), runIdent, runModuleName, showIdent, showQualified)
import Language.PureScript.Options (CodegenTarget(..), Options(..))
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)
import Language.PureScript.Constants.Prim qualified as C

import System.FilePath.Posix ((</>))

-- | Generate code in the simplified JavaScript intermediate representation for all declarations in a
-- module.
moduleToJs
  :: forall m
   . (MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe PSString
  -> m AST.Module
moduleToJs (Module _ coms mn _ imps exps reExps foreigns decls) foreignInclude =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let imps' = ordNub $ map snd imps
    let mnLookup = renameImports usedNames imps'
    (jsDecls, Any needRuntimeLazy) <- runWriterT $ mapM (moduleBindToJs mn) decls
    optimized <- fmap (fmap (fmap annotatePure)) . optimize (map identToJs exps) $ if needRuntimeLazy then [runtimeLazy] : jsDecls else jsDecls
    F.traverse_ (F.traverse_ checkIntegers) optimized
    comments <- not <$> asks optionsNoComments
    let header = if comments then coms else []
    let foreign' = maybe [] (pure . AST.Import FFINamespace) $ if null foreigns then Nothing else foreignInclude
    let moduleBody = concat optimized
    let (S.union (M.keysSet reExps) -> usedModuleNames, renamedModuleBody) = traverse (replaceModuleAccessors mnLookup) moduleBody
    let jsImports
          = map (importToJs mnLookup)
          . filter (flip S.member usedModuleNames)
          $ (\\ (mn : C.primModules)) imps'
    let foreignExps = exps `intersect` foreigns
    let standardExps = exps \\ foreignExps
    let reExps' = M.toList (M.withoutKeys reExps (S.fromList C.primModules))
    let jsExports
          =  (maybeToList . exportsToJs foreignInclude $ foreignExps)
          ++ (maybeToList . exportsToJs Nothing $ standardExps)
          ++  mapMaybe reExportsToJs reExps'
    return $ AST.Module header (foreign' ++ jsImports) renamedModuleBody jsExports

  where
  -- Adds purity annotations to top-level values for bundlers.
  -- The semantics here derive from treating top-level module evaluation as pure, which lets
  -- us remove any unreferenced top-level declarations. To achieve this, we wrap any non-trivial
  -- top-level values in an IIFE marked with a pure annotation.
  annotatePure :: AST -> AST
  annotatePure = annotateOrWrap
    where
    annotateOrWrap = liftA2 fromMaybe pureIife maybePure

    -- If the JS is potentially effectful (in the eyes of a bundler that
    -- doesn't know about PureScript), return Nothing. Otherwise, return Just
    -- the JS with any needed pure annotations added, and, in the case of a
    -- variable declaration, an IIFE to be annotated.
    maybePure :: AST -> Maybe AST
    maybePure = maybePureGen False

    -- Like maybePure, but doesn't add a pure annotation to App. This exists
    -- to prevent from doubling up on annotation comments on curried
    -- applications; from experimentation, it turns out that a comment on the
    -- outermost App is sufficient for the entire curried chain to be
    -- considered effect-free.
    maybePure' :: AST -> Maybe AST
    maybePure' = maybePureGen True

    maybePureGen alreadyAnnotated = \case
      AST.VariableIntroduction ss name j -> Just (AST.VariableIntroduction ss name (fmap annotateOrWrap <$> j))
      AST.App ss f args -> (if alreadyAnnotated then AST.App else pureApp) ss <$> maybePure' f <*> traverse maybePure args
      AST.ArrayLiteral ss jss -> AST.ArrayLiteral ss <$> traverse maybePure jss
      AST.ObjectLiteral ss props -> AST.ObjectLiteral ss <$> traverse (traverse maybePure) props
      AST.Comment c js -> AST.Comment c <$> maybePure js

      js@(AST.Indexer _ _ (AST.Var _ FFINamespace)) -> Just js

      js@AST.NumericLiteral{} -> Just js
      js@AST.StringLiteral{}  -> Just js
      js@AST.BooleanLiteral{} -> Just js
      js@AST.Function{}       -> Just js
      js@AST.Var{}            -> Just js
      js@AST.ModuleAccessor{} -> Just js

      _ -> Nothing

    pureIife :: AST -> AST
    pureIife val = pureApp Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing [AST.Return Nothing val])) []

    pureApp :: Maybe SourceSpan -> AST -> [AST] -> AST
    pureApp ss f = AST.Comment AST.PureAnnotation . AST.App ss f

  -- Extracts all declaration names from a binding group.
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  renameImports :: [Ident] -> [ModuleName] -> M.Map ModuleName Text
  renameImports = go M.empty
    where
    go :: M.Map ModuleName Text -> [Ident] -> [ModuleName] -> M.Map ModuleName Text
    go acc used (mn' : mns') =
      let mnj = moduleNameToJs mn'
      in if mn' /= mn && Ident mnj `elem` used
         then let newName = freshModuleName 1 mnj used
              in go (M.insert mn' newName acc) (Ident newName : used) mns'
         else go (M.insert mn' mnj acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> Text -> [Ident] -> Text
    freshModuleName i mn' used =
      let newName = mn' <> "_" <> T.pack (show i)
      in if Ident newName `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- Generates JavaScript code for a module import, binding the required module
  -- to the alternative
  importToJs :: M.Map ModuleName Text -> ModuleName -> AST.Import
  importToJs mnLookup mn' =
    let mnSafe = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    in AST.Import mnSafe (moduleImportPath mn')

  -- Generates JavaScript code for exporting at least one identifier,
  -- eventually from another module.
  exportsToJs :: Maybe PSString -> [Ident] -> Maybe AST.Export
  exportsToJs from = fmap (flip AST.Export from) . NEL.nonEmpty . fmap runIdent

  -- Generates JavaScript code for re-exporting at least one identifier from
  -- from another module.
  reExportsToJs :: (ModuleName, [Ident]) -> Maybe AST.Export
  reExportsToJs = uncurry exportsToJs . first (Just . moduleImportPath)

  moduleImportPath :: ModuleName -> PSString
  moduleImportPath mn' = fromString (".." </> T.unpack (runModuleName mn') </> "index.js")

  -- Replaces the `ModuleAccessor`s in the AST with `Indexer`s, ensuring that
  -- the generated code refers to the collision-avoiding renamed module
  -- imports. Also returns set of used module names.
  replaceModuleAccessors :: M.Map ModuleName Text -> AST -> (S.Set ModuleName, AST)
  replaceModuleAccessors mnLookup = everywhereTopDownM $ \case
    AST.ModuleAccessor _ mn' name ->
      let mnSafe = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
      in (S.singleton mn', accessorString name $ AST.Var Nothing mnSafe)
    other -> pure other

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
    AST.VariableIntroduction Nothing "$runtime_lazy" . Just . (UnknownEffects, ) . AST.Function Nothing Nothing ["name", "moduleName", "init"] . AST.Block Nothing $
      [ AST.VariableIntroduction Nothing "state" . Just . (UnknownEffects, ) . AST.NumericLiteral Nothing $ Left 0
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
   . (MonadReader Options m, MonadSupply m, MonadWriter Any m, MonadError MultipleErrors m)
  => ModuleName
  -> Bind Ann
  -> m [AST]
moduleBindToJs mn = bindToJs
  where
  -- Generate code in the simplified JavaScript intermediate representation for a declaration
  bindToJs :: Bind Ann -> m [AST]
  bindToJs (NonRec (_, _, Just IsTypeClassConstructor) _ _) = pure []
    -- Unlike other newtype constructors, type class constructors are only
    -- ever applied; it's not possible to use them as values. So it's safe to
    -- erase them.
  bindToJs (NonRec ann ident val) = return <$> nonRecToJS ann ident val
  bindToJs (Rec vals) = writer (applyLazinessTransform mn vals) >>= traverse (uncurry . uncurry $ nonRecToJS)

  -- Generate code in the simplified JavaScript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  nonRecToJS :: Ann -> Ident -> Expr Ann -> m AST
  nonRecToJS a i e@(extractAnn -> (_, com, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS a i (modifyAnn removeComments e)
       else AST.Comment (AST.SourceComments com) <$> nonRecToJS a i (modifyAnn removeComments e)
  nonRecToJS (ss, _, _) ident val = do
    js <- valueToJs val
    withPos ss $ AST.VariableIntroduction Nothing (identToJs ident) (Just (guessEffects val, js))

  guessEffects :: Expr Ann -> AST.InitializerEffects
  guessEffects = \case
    Var _ (Qualified (BySourcePos _) _) -> NoEffects
    App (_, _, Just IsSyntheticApp) _ _ -> NoEffects
    _                                   -> UnknownEffects

  withPos :: SourceSpan -> AST -> m AST
  withPos ss js = do
    withSM <- asks (elem JSSourceMap . optionsCodegenTargets)
    return $ if withSM
      then withSourceSpan ss js
      else js

  -- Generate code in the simplified JavaScript intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToJs

  -- Generate code in the simplified JavaScript intermediate representation for a value or expression.
  valueToJs :: Expr Ann -> m AST
  valueToJs e =
    let (ss, _, _) = extractAnn e in
    withPos ss =<< valueToJs' e

  valueToJs' :: Expr Ann -> m AST
  valueToJs' (Literal (pos, _, _) l) =
    rethrowWithPosition pos $ literalToValueJS pos l
  valueToJs' (Var (_, _, Just (IsConstructor _ [])) name) =
    return $ accessorString "value" $ qualifiedToJS id name
  valueToJs' (Var (_, _, Just (IsConstructor _ _)) name) =
    return $ accessorString "create" $ qualifiedToJS id name
  valueToJs' (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs' (ObjectUpdate (pos, _, _) o copy ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    case copy of
      Nothing -> extendObj obj sts
      Just names -> pure $ AST.ObjectLiteral (Just pos) (map f names ++ sts)
        where f name = (name, accessorString name obj)
  valueToJs' (Abs _ arg val) = do
    ret <- valueToJs val
    let jsArg = case arg of
                  UnusedIdent -> []
                  _           -> [identToJs arg]
    return $ AST.Function Nothing Nothing jsArg (AST.Block Nothing [AST.Return Nothing ret])
  valueToJs' e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ AST.Unary Nothing AST.New $ AST.App Nothing (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> AST.App Nothing fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs' (Var (_, _, Just IsForeign) qi@(Qualified (ByModuleName mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  valueToJs' (Var (_, _, Just IsForeign) ident) =
    internalError $ "Encountered an unqualified reference to a foreign ident " ++ T.unpack (showQualified showIdent ident)
  valueToJs' (Var _ ident) = return $ varToJs ident
  valueToJs' (Case (ss, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs ss binders vals
  valueToJs' (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (ds' ++ [AST.Return Nothing ret]))) []
  valueToJs' (Constructor (_, _, Just IsNewtype) _ ctor _) =
    return $ AST.VariableIntroduction Nothing (properToJs ctor) (Just . (UnknownEffects, ) $
                AST.ObjectLiteral Nothing [("create",
                  AST.Function Nothing Nothing ["value"]
                    (AST.Block Nothing [AST.Return Nothing $ AST.Var Nothing "value"]))])
  valueToJs' (Constructor _ _ ctor []) =
    return $ iife (properToJs ctor) [ AST.Function Nothing (Just (properToJs ctor)) [] (AST.Block Nothing [])
           , AST.Assignment Nothing (accessorString "value" (AST.Var Nothing (properToJs ctor)))
                (AST.Unary Nothing AST.New $ AST.App Nothing (AST.Var Nothing (properToJs ctor)) []) ]
  valueToJs' (Constructor _ _ ctor fields) =
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

  -- Shallow copy an object.
  extendObj :: AST -> [(PSString, AST)] -> m AST
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    evaluatedObj <- freshName
    let
      jsKey = AST.Var Nothing key
      jsNewObj = AST.Var Nothing newObj
      jsEvaluatedObj = AST.Var Nothing evaluatedObj
      block = AST.Block Nothing (evaluate:objAssign:copy:extend ++ [AST.Return Nothing jsNewObj])
      evaluate = AST.VariableIntroduction Nothing evaluatedObj (Just (UnknownEffects, obj))
      objAssign = AST.VariableIntroduction Nothing newObj (Just (NoEffects, AST.ObjectLiteral Nothing []))
      copy = AST.ForIn Nothing key jsEvaluatedObj $ AST.Block Nothing [AST.IfElse Nothing cond assign Nothing]
      cond = AST.App Nothing (accessorString "call" (accessorString "hasOwnProperty" (AST.ObjectLiteral Nothing []))) [jsEvaluatedObj, jsKey]
      assign = AST.Block Nothing [AST.Assignment Nothing (AST.Indexer Nothing jsKey jsNewObj) (AST.Indexer Nothing jsKey jsEvaluatedObj)]
      stToAssign (s, js) = AST.Assignment Nothing (accessorString s jsNewObj) js
      extend = map stToAssign sts
    return $ AST.App Nothing (AST.Function Nothing Nothing [] block) []

  -- Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable.
  varToJs :: Qualified Ident -> AST
  varToJs (Qualified (BySourcePos _) ident) = var ident
  varToJs qual = qualifiedToJS id qual

  -- Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToJS :: (a -> Ident) -> Qualified a -> AST
  qualifiedToJS f (Qualified (ByModuleName C.M_Prim) a) = AST.Var Nothing . runIdent $ f a
  qualifiedToJS f (Qualified (ByModuleName mn') a) | mn /= mn' = AST.ModuleAccessor Nothing mn' . mkString . T.concatMap identCharToText . runIdent $ f a
  qualifiedToJS f (Qualified _ a) = AST.Var Nothing $ identToJs (f a)

  foreignIdent :: Ident -> AST
  foreignIdent ident = accessorString (mkString $ runIdent ident) (AST.Var Nothing FFINamespace)

  -- Generate code in the simplified JavaScript intermediate representation for pattern match binders
  -- and guards.
  bindersToJs :: SourceSpan -> [CaseAlternative Ann] -> [AST] -> m AST
  bindersToJs ss binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (AST.VariableIntroduction Nothing) valNames (map (Just . (UnknownEffects, )) vals)
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

  binderToJs :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs s done binder =
    let (ss, _, _) = extractBinderAnn binder in
    traverse (withPos ss) =<< binderToJs' s done binder

  -- Generate code in the simplified JavaScript intermediate representation for a pattern match
  -- binder.
  binderToJs' :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs' _ done NullBinder{} = return done
  binderToJs' varName done (LiteralBinder _ l) =
    literalToBinderJS varName done l
  binderToJs' varName done (VarBinder _ ident) =
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (NoEffects, AST.Var Nothing varName)) : done)
  binderToJs' varName done (ConstructorBinder (_, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (_, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    js <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> js
      SumType ->
        [AST.IfElse Nothing (AST.InstanceOf Nothing (AST.Var Nothing varName) (qualifiedToJS (Ident . runProperName) ctor))
                  (AST.Block Nothing js)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [AST] -> m [AST]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      js <- binderToJs argVar done'' binder
      return (AST.VariableIntroduction Nothing argVar (Just (UnknownEffects, accessorString (mkString $ identToJs field) $ AST.Var Nothing varName)) : js)
  binderToJs' _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (NoEffects, AST.Var Nothing varName)) : js)

  literalToBinderJS :: Text -> [AST] -> Literal (Binder Ann) -> m [AST]
  literalToBinderJS varName done (NumericLiteral num) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.NumericLiteral Nothing num)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (CharLiteral c) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing (fromString [c]))) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (StringLiteral str) =
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (AST.Var Nothing varName) (AST.StringLiteral Nothing str)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral True) =
    return [AST.IfElse Nothing (AST.Var Nothing varName) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral False) =
    return [AST.IfElse Nothing (AST.Unary Nothing AST.Not (AST.Var Nothing varName)) (AST.Block Nothing done) Nothing]
  literalToBinderJS varName done (ObjectLiteral bs) = go done bs
    where
    go :: [AST] -> [(PSString, Binder Ann)] -> m [AST]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      js <- binderToJs propVar done'' binder
      return (AST.VariableIntroduction Nothing propVar (Just (UnknownEffects, accessorString prop (AST.Var Nothing varName))) : js)
  literalToBinderJS varName done (ArrayLiteral bs) = do
    js <- go done 0 bs
    return [AST.IfElse Nothing (AST.Binary Nothing AST.EqualTo (accessorString "length" (AST.Var Nothing varName)) (AST.NumericLiteral Nothing (Left (fromIntegral $ length bs)))) (AST.Block Nothing js) Nothing]
    where
    go :: [AST] -> Integer -> [Binder Ann] -> m [AST]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      js <- binderToJs elVar done'' binder
      return (AST.VariableIntroduction Nothing elVar (Just (UnknownEffects, AST.Indexer Nothing (AST.NumericLiteral Nothing (Left index)) (AST.Var Nothing varName))) : js)

accessorString :: PSString -> AST -> AST
accessorString prop = AST.Indexer Nothing (AST.StringLiteral Nothing prop)

pattern FFINamespace :: Text
pattern FFINamespace = "$foreign"
