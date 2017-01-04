-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  ) where

import Prelude.Compat

import Control.Arrow ((&&&))
import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Data.List ((\\), delete, intersect, nub)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow,
                                   errorMessage, rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.PSString (PSString, mkString, decodeString)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import System.FilePath.Posix ((</>))

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe JS
  -> m [JS]
moduleToJs (Module coms mn imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    jsImports <- traverse (importToJs mnLookup) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ nub $ map snd imps
    let decls' = renameModules mnLookup decls
    jsDecls <- mapM bindToJs decls'
    optimized <- traverse (traverse optimize) jsDecls
    F.traverse_ (F.traverse_ checkIntegers) optimized
    comments <- not <$> asks optionsNoComments
    let strict = JSStringLiteral Nothing "use strict"
    let header = if comments && not (null coms) then JSComment Nothing coms strict else strict
    let foreign' = [JSVariableIntroduction Nothing "$foreign" foreign_ | not $ null foreigns || isNothing foreign_]
    let moduleBody = header : foreign' ++ jsImports ++ concat optimized
    let foreignExps = exps `intersect` (fst `map` foreigns)
    let standardExps = exps \\ foreignExps
    let exps' = JSObjectLiteral Nothing $ map (mkString . runIdent &&& JSVar Nothing . identToJs) standardExps
                               ++ map (mkString . runIdent &&& foreignIdent) foreignExps
    return $ moduleBody ++ [JSAssignment Nothing (JSAccessor Nothing "exports" (JSVar Nothing "module")) exps']

  where

  -- |
  -- Extracts all declaration names from a binding group.
  --
  getNames :: Bind Ann -> [Ident]
  getNames (NonRec _ ident _) = [ident]
  getNames (Rec vals) = map (snd . fst) vals

  -- |
  -- Creates alternative names for each module to ensure they don't collide
  -- with declaration names.
  --
  renameImports :: [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
  renameImports = go M.empty
    where
    go :: M.Map ModuleName (Ann, ModuleName) -> [Ident] -> [(Ann, ModuleName)] -> M.Map ModuleName (Ann, ModuleName)
    go acc used ((ann, mn') : mns') =
      let mni = Ident $ runModuleName mn'
      in if mn' /= mn && mni `elem` used
         then let newName = freshModuleName 1 mn' used
              in go (M.insert mn' (ann, newName) acc) (Ident (runModuleName newName) : used) mns'
         else go (M.insert mn' (ann, mn') acc) used mns'
    go acc _ [] = acc

    freshModuleName :: Integer -> ModuleName -> [Ident] -> ModuleName
    freshModuleName i mn'@(ModuleName pns) used =
      let newName = ModuleName $ init pns ++ [ProperName $ runProperName (last pns) <> "_" <> T.pack (show i)]
      in if Ident (runModuleName newName) `elem` used
         then freshModuleName (i + 1) mn' used
         else newName

  -- |
  -- Generates Javascript code for a module import, binding the required module
  -- to the alternative
  --
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m JS
  importToJs mnLookup mn' = do
    let ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    let moduleBody = JSApp Nothing (JSVar Nothing "require") [JSStringLiteral Nothing (fromString (".." </> T.unpack (runModuleName mn')))]
    withPos ss $ JSVariableIntroduction Nothing (moduleNameToJs mnSafe) (Just moduleBody)

  -- |
  -- Replaces the `ModuleName`s in the AST so that the generated code refers to
  -- the collision-avoiding renamed module imports.
  --
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
  -- Generate code in the simplified Javascript intermediate representation for a declaration
  --
  bindToJs :: Bind Ann -> m [JS]
  bindToJs (NonRec ann ident val) = return <$> nonRecToJS ann ident val
  bindToJs (Rec vals) = forM vals (uncurry . uncurry $ nonRecToJS)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  --
  nonRecToJS :: Ann -> Ident -> Expr Ann -> m JS
  nonRecToJS a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS a i (modifyAnn removeComments e)
       else JSComment Nothing com <$> nonRecToJS a i (modifyAnn removeComments e)
  nonRecToJS (ss, _, _, _) ident val = do
    js <- valueToJs val
    withPos ss $ JSVariableIntroduction Nothing (identToJs ident) (Just js)

  withPos :: Maybe SourceSpan -> JS -> m JS
  withPos (Just ss) js = do
    withSM <- asks optionsSourceMaps
    return $ if withSM
      then withSourceSpan ss js
      else js
  withPos Nothing js = return js

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a variable based on a
  -- PureScript identifier.
  --
  var :: Ident -> JS
  var = JSVar Nothing . identToJs

  -- |
  -- Generate code in the simplified Javascript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
  -- indexer is returned.
  --
  accessor :: Ident -> JS -> JS
  accessor (Ident prop) = accessorString $ mkString prop
  accessor (GenIdent _ _) = internalError "GenIdent in accessor"

  accessorString :: PSString -> JS -> JS
  accessorString prop =
    case decodeString prop of
      Just s | not (identNeedsEscaping s) ->
        JSAccessor Nothing s
      _ ->
        JSIndexer Nothing (JSStringLiteral Nothing prop)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a value or expression.
  --
  valueToJs :: Expr Ann -> m JS
  valueToJs e =
    let (ss, _, _, _) = extractAnn e in
    withPos ss =<< valueToJs' e

  valueToJs' :: Expr Ann -> m JS
  valueToJs' (Literal (pos, _, _, _) l) =
    maybe id rethrowWithPosition pos $ literalToValueJS l
  valueToJs' (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ JSAccessor Nothing "value" $ qualifiedToJS id name
  valueToJs' (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ JSAccessor Nothing "create" $ qualifiedToJS id name
  valueToJs' (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs' (ObjectUpdate _ o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj obj sts
  valueToJs' e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let args = unAbs e
    in return $ JSFunction Nothing Nothing (map identToJs args) (JSBlock Nothing $ map assign args)
    where
    unAbs :: Expr Ann -> [Ident]
    unAbs (Abs _ arg val) = arg : unAbs val
    unAbs _ = []
    assign :: Ident -> JS
    assign name = JSAssignment Nothing (accessorString (mkString $ runIdent name) (JSVar Nothing "this"))
                               (var name)
  valueToJs' (Abs _ arg val) = do
    ret <- valueToJs val
    return $ JSFunction Nothing Nothing [identToJs arg] (JSBlock Nothing [JSReturn Nothing ret])
  valueToJs' e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ JSUnary Nothing JSNew $ JSApp Nothing (qualifiedToJS id name) args'
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ JSUnary Nothing JSNew $ JSApp Nothing (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> JSApp Nothing fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs' (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  valueToJs' (Var (_, _, _, Just IsForeign) ident) =
    internalError $ "Encountered an unqualified reference to a foreign ident " ++ T.unpack (showQualified showIdent ident)
  valueToJs' (Var _ ident) = return $ varToJs ident
  valueToJs' (Case (maybeSpan, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs maybeSpan binders vals
  valueToJs' (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ JSApp Nothing (JSFunction Nothing Nothing [] (JSBlock Nothing (ds' ++ [JSReturn Nothing ret]))) []
  valueToJs' (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
    return $ JSVariableIntroduction Nothing (properToJs ctor) (Just $
                JSObjectLiteral Nothing [("create",
                  JSFunction Nothing Nothing ["value"]
                    (JSBlock Nothing [JSReturn Nothing $ JSVar Nothing "value"]))])
  valueToJs' (Constructor _ _ (ProperName ctor) []) =
    return $ iife (properToJs ctor) [ JSFunction Nothing (Just (properToJs ctor)) [] (JSBlock Nothing [])
           , JSAssignment Nothing (JSAccessor Nothing "value" (JSVar Nothing (properToJs ctor)))
                (JSUnary Nothing JSNew $ JSApp Nothing (JSVar Nothing (properToJs ctor)) []) ]
  valueToJs' (Constructor _ _ (ProperName ctor) fields) =
    let constructor =
          let body = [ JSAssignment Nothing (JSAccessor Nothing (identToJs f) (JSVar Nothing "this")) (var f) | f <- fields ]
          in JSFunction Nothing (Just (properToJs ctor)) (identToJs `map` fields) (JSBlock Nothing body)
        createFn =
          let body = JSUnary Nothing JSNew $ JSApp Nothing (JSVar Nothing (properToJs ctor)) (var `map` fields)
          in foldr (\f inner -> JSFunction Nothing Nothing [identToJs f] (JSBlock Nothing [JSReturn Nothing inner])) body fields
    in return $ iife (properToJs ctor) [ constructor
                          , JSAssignment Nothing (JSAccessor Nothing "create" (JSVar Nothing (properToJs ctor))) createFn
                          ]

  iife :: Text -> [JS] -> JS
  iife v exprs = JSApp Nothing (JSFunction Nothing Nothing [] (JSBlock Nothing $ exprs ++ [JSReturn Nothing $ JSVar Nothing v])) []

  literalToValueJS :: Literal (Expr Ann) -> m JS
  literalToValueJS (NumericLiteral (Left i)) = return $ JSNumericLiteral Nothing (Left i)
  literalToValueJS (NumericLiteral (Right n)) = return $ JSNumericLiteral Nothing (Right n)
  literalToValueJS (StringLiteral s) = return $ JSStringLiteral Nothing s
  literalToValueJS (CharLiteral c) = return $ JSStringLiteral Nothing (fromString [c])
  literalToValueJS (BooleanLiteral b) = return $ JSBooleanLiteral Nothing b
  literalToValueJS (ArrayLiteral xs) = JSArrayLiteral Nothing <$> mapM valueToJs xs
  literalToValueJS (ObjectLiteral ps) = JSObjectLiteral Nothing <$> mapM (sndM valueToJs) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: JS -> [(PSString, JS)] -> m JS
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    evaluatedObj <- freshName
    let
      jsKey = JSVar Nothing key
      jsNewObj = JSVar Nothing newObj
      jsEvaluatedObj = JSVar Nothing evaluatedObj
      block = JSBlock Nothing (evaluate:objAssign:copy:extend ++ [JSReturn Nothing jsNewObj])
      evaluate = JSVariableIntroduction Nothing evaluatedObj (Just obj)
      objAssign = JSVariableIntroduction Nothing newObj (Just $ JSObjectLiteral Nothing [])
      copy = JSForIn Nothing key jsEvaluatedObj $ JSBlock Nothing [JSIfElse Nothing cond assign Nothing]
      cond = JSApp Nothing (JSAccessor Nothing "call" (JSAccessor Nothing "hasOwnProperty" (JSObjectLiteral Nothing []))) [jsEvaluatedObj, jsKey]
      assign = JSBlock Nothing [JSAssignment Nothing (JSIndexer Nothing jsKey jsNewObj) (JSIndexer Nothing jsKey jsEvaluatedObj)]
      stToAssign (s, js) = JSAssignment Nothing (accessorString s jsNewObj) js
      extend = map stToAssign sts
    return $ JSApp Nothing (JSFunction Nothing Nothing [] block) []

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable.
  --
  varToJs :: Qualified Ident -> JS
  varToJs (Qualified Nothing ident) = var ident
  varToJs qual = qualifiedToJS id qual

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  --
  qualifiedToJS :: (a -> Ident) -> Qualified a -> JS
  qualifiedToJS f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = JSVar Nothing . runIdent $ f a
  qualifiedToJS f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (JSVar Nothing (moduleNameToJs mn'))
  qualifiedToJS f (Qualified _ a) = JSVar Nothing $ identToJs (f a)

  foreignIdent :: Ident -> JS
  foreignIdent ident = accessorString (mkString $ runIdent ident) (JSVar Nothing "$foreign")

  -- |
  -- Generate code in the simplified Javascript intermediate representation for pattern match binders
  -- and guards.
  --
  bindersToJs :: Maybe SourceSpan -> [CaseAlternative Ann] -> [JS] -> m JS
  bindersToJs maybeSpan binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith (JSVariableIntroduction Nothing) valNames (map Just vals)
    jss <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToJs result
      go valNames ret bs
    return $ JSApp Nothing (JSFunction Nothing Nothing [] (JSBlock Nothing (assignments ++ concat jss ++ [JSThrow Nothing $ failedPatternError valNames])))
                   []
    where
      go :: [Text] -> [JS] -> [Binder Ann] -> m [JS]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToJs v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToJs"

      failedPatternError :: [Text] -> JS
      failedPatternError names = JSUnary Nothing JSNew $ JSApp Nothing (JSVar Nothing "Error") [JSBinary Nothing Add (JSStringLiteral Nothing $ mkString failedPatternMessage) (JSArrayLiteral Nothing $ zipWith valueError names vals)]

      failedPatternMessage :: Text
      failedPatternMessage = "Failed pattern match" <> maybe "" (((" at " <> runModuleName mn <> " ") <>) . displayStartEndPos) maybeSpan <> ": "

      valueError :: Text -> JS -> JS
      valueError _ l@(JSNumericLiteral _ _) = l
      valueError _ l@(JSStringLiteral _ _)  = l
      valueError _ l@(JSBooleanLiteral _ _) = l
      valueError s _                        = JSAccessor Nothing "name" . JSAccessor Nothing "constructor" $ JSVar Nothing s

      guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [JS]
      guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToJs cond
        done  <- valueToJs val
        return $ JSIfElse Nothing cond' (JSBlock Nothing [JSReturn Nothing done]) Nothing
      guardsToJs (Right v) = return . JSReturn Nothing <$> valueToJs v

  binderToJs :: Text -> [JS] -> Binder Ann -> m [JS]
  binderToJs s done binder =
    let (ss, _, _, _) = extractBinderAnn binder in
    traverse (withPos ss) =<< binderToJs' s done binder

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a pattern match
  -- binder.
  --
  binderToJs' :: Text -> [JS] -> Binder Ann -> m [JS]
  binderToJs' _ done NullBinder{} = return done
  binderToJs' varName done (LiteralBinder _ l) =
    literalToBinderJS varName done l
  binderToJs' varName done (VarBinder _ ident) =
    return (JSVariableIntroduction Nothing (identToJs ident) (Just (JSVar Nothing varName)) : done)
  binderToJs' varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    js <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> js
      SumType ->
        [JSIfElse Nothing (JSInstanceOf Nothing (JSVar Nothing varName) (qualifiedToJS (Ident . runProperName) ctor))
                  (JSBlock Nothing js)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [JS] -> m [JS]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      js <- binderToJs argVar done'' binder
      return (JSVariableIntroduction Nothing argVar (Just (JSAccessor Nothing (identToJs field) (JSVar Nothing varName))) : js)
  binderToJs' _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (JSVariableIntroduction Nothing (identToJs ident) (Just (JSVar Nothing varName)) : js)

  literalToBinderJS :: Text -> [JS] -> Literal (Binder Ann) -> m [JS]
  literalToBinderJS varName done (NumericLiteral num) =
    return [JSIfElse Nothing (JSBinary Nothing EqualTo (JSVar Nothing varName) (JSNumericLiteral Nothing num)) (JSBlock Nothing done) Nothing]
  literalToBinderJS varName done (CharLiteral c) =
    return [JSIfElse Nothing (JSBinary Nothing EqualTo (JSVar Nothing varName) (JSStringLiteral Nothing (fromString [c]))) (JSBlock Nothing done) Nothing]
  literalToBinderJS varName done (StringLiteral str) =
    return [JSIfElse Nothing (JSBinary Nothing EqualTo (JSVar Nothing varName) (JSStringLiteral Nothing str)) (JSBlock Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral True) =
    return [JSIfElse Nothing (JSVar Nothing varName) (JSBlock Nothing done) Nothing]
  literalToBinderJS varName done (BooleanLiteral False) =
    return [JSIfElse Nothing (JSUnary Nothing Not (JSVar Nothing varName)) (JSBlock Nothing done) Nothing]
  literalToBinderJS varName done (ObjectLiteral bs) = go done bs
    where
    go :: [JS] -> [(PSString, Binder Ann)] -> m [JS]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      js <- binderToJs propVar done'' binder
      return (JSVariableIntroduction Nothing propVar (Just (accessorString prop (JSVar Nothing varName))) : js)
  literalToBinderJS varName done (ArrayLiteral bs) = do
    js <- go done 0 bs
    return [JSIfElse Nothing (JSBinary Nothing EqualTo (JSAccessor Nothing "length" (JSVar Nothing varName)) (JSNumericLiteral Nothing (Left (fromIntegral $ length bs)))) (JSBlock Nothing js) Nothing]
    where
    go :: [JS] -> Integer -> [Binder Ann] -> m [JS]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      js <- binderToJs elVar done'' binder
      return (JSVariableIntroduction Nothing elVar (Just (JSIndexer Nothing (JSNumericLiteral Nothing (Left index)) (JSVar Nothing varName))) : js)

  -- Check that all integers fall within the valid int range for JavaScript.
  checkIntegers :: JS -> m ()
  checkIntegers = void . everywhereOnJSTopDownM go
    where
    go :: JS -> m JS
    go (JSUnary _ Negate (JSNumericLiteral ss (Left i))) =
      -- Move the negation inside the literal; since this is a top-down
      -- traversal doing this replacement will stop the next case from raising
      -- the error when attempting to use -2147483648, as if left unrewritten
      -- the value is `JSUnary Negate (JSNumericLiteral (Left 2147483648))`, and
      -- 2147483648 is larger than the maximum allowed int.
      return $ JSNumericLiteral ss (Left (-i))
    go js@(JSNumericLiteral _ (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError . errorMessage $ IntOutOfRange i "JavaScript" minInt maxInt
         else return js
    go other = return other
