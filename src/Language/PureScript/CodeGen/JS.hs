-- | This module generates code in the core imperative representation from
-- elaborated PureScript code.
module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  ) where

import Prelude.Compat
import Protolude (ordNub)

import Control.Arrow ((&&&))
import Control.Monad (forM, replicateM, void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Data.List ((\\), delete, intersect)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T

import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreImp.AST (AST, everywhereTopDownM, withSourceSpan)
import qualified Language.PureScript.CoreImp.AST as AST
import Language.PureScript.CoreImp.Optimizer
import Language.PureScript.CoreFn
import Language.PureScript.Crash
import Language.PureScript.Errors (ErrorMessageHint(..), SimpleErrorMessage(..),
                                   MultipleErrors(..), rethrow,
                                   errorMessage, rethrowWithPosition, addHint)
import Language.PureScript.Names
import Language.PureScript.Options
import Language.PureScript.PSString (PSString, mkString)
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import System.FilePath.Posix ((</>))

-- | Generate code in the simplified JavaScript intermediate representation for all declarations in a
-- module.
moduleToJs
  :: forall m
   . (Monad m, MonadReader Options m, MonadSupply m, MonadError MultipleErrors m)
  => Module Ann
  -> Maybe AST
  -> m [AST]
moduleToJs (Module coms mn imps exps foreigns decls) foreign_ =
  rethrow (addHint (ErrorInModule mn)) $ do
    let usedNames = concatMap getNames decls
    let mnLookup = renameImports usedNames imps
    jsImports <- traverse (importToJs mnLookup) . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ ordNub $ map snd imps
    let decls' = renameModules mnLookup decls
    jsDecls <- mapM bindToJs decls'
    optimized <- traverse (traverse optimize) jsDecls
    F.traverse_ (F.traverse_ checkIntegers) optimized
    comments <- not <$> asks optionsNoComments
    let strict = AST.StringLiteral Nothing "use strict"
    let header = if comments && not (null coms) then AST.Comment Nothing coms strict else strict
    let foreign' = [AST.VariableIntroduction Nothing "$foreign" foreign_ | not $ null foreigns || isNothing foreign_]
    let moduleBody = header : foreign' ++ jsImports ++ concat optimized
    let foreignExps = exps `intersect` (fst `map` foreigns)
    let standardExps = exps \\ foreignExps
    let exps' = AST.ObjectLiteral Nothing $ map (mkString . runIdent &&& AST.Var Nothing . identToJs) standardExps
                               ++ map (mkString . runIdent &&& foreignIdent) foreignExps
    return $ moduleBody ++ [AST.Assignment Nothing (accessorString "exports" (AST.Var Nothing "module")) exps']

  where

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

  -- | Generates JavaScript code for a module import, binding the required module
  -- to the alternative
  importToJs :: M.Map ModuleName (Ann, ModuleName) -> ModuleName -> m AST
  importToJs mnLookup mn' = do
    let ((ss, _, _, _), mnSafe) = fromMaybe (internalError "Missing value in mnLookup") $ M.lookup mn' mnLookup
    let moduleBody = AST.App Nothing (AST.Var Nothing "require") [AST.StringLiteral Nothing (fromString (".." </> T.unpack (runModuleName mn')))]
    withPos ss $ AST.VariableIntroduction Nothing (moduleNameToJs mnSafe) (Just moduleBody)

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
  -- Generate code in the simplified JavaScript intermediate representation for a declaration
  --
  bindToJs :: Bind Ann -> m [AST]
  bindToJs (NonRec ann ident val) = return <$> nonRecToJS ann ident val
  bindToJs (Rec vals) = forM vals (uncurry . uncurry $ nonRecToJS)

  -- | Generate code in the simplified JavaScript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  nonRecToJS :: Ann -> Ident -> Expr Ann -> m AST
  nonRecToJS a i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS a i (modifyAnn removeComments e)
       else AST.Comment Nothing com <$> nonRecToJS a i (modifyAnn removeComments e)
  nonRecToJS (ss, _, _, _) ident val = do
    js <- valueToJs val
    withPos ss $ AST.VariableIntroduction Nothing (identToJs ident) (Just js)

  withPos :: SourceSpan -> AST -> m AST
  withPos ss js = do
    withSM <- asks optionsSourceMaps
    return $ if withSM
      then withSourceSpan ss js
      else js

  -- | Generate code in the simplified JavaScript intermediate representation for a variable based on a
  -- PureScript identifier.
  var :: Ident -> AST
  var = AST.Var Nothing . identToJs

  -- | Generate code in the simplified JavaScript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in JavaScript (symbol based, reserved name) an
  -- indexer is returned.
  accessor :: Ident -> AST -> AST
  accessor (Ident prop) = accessorString $ mkString prop
  accessor (GenIdent _ _) = internalError "GenIdent in accessor"

  accessorString :: PSString -> AST -> AST
  accessorString prop = AST.Indexer Nothing (AST.StringLiteral Nothing prop)

  -- | Generate code in the simplified JavaScript intermediate representation for a value or expression.
  valueToJs :: Expr Ann -> m AST
  valueToJs e =
    let (ss, _, _, _) = extractAnn e in
    withPos ss =<< valueToJs' e

  valueToJs' :: Expr Ann -> m AST
  valueToJs' (Literal (pos, _, _, _) l) =
    rethrowWithPosition pos $ literalToValueJS l
  valueToJs' (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ accessorString "value" $ qualifiedToJS id name
  valueToJs' (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ accessorString "create" $ qualifiedToJS id name
  valueToJs' (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs' (ObjectUpdate _ o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj obj sts
  valueToJs' e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let args = unAbs e
    in return $ AST.Function Nothing Nothing (map identToJs args) (AST.Block Nothing $ map assign args)
    where
    unAbs :: Expr Ann -> [Ident]
    unAbs (Abs _ arg val) = arg : unAbs val
    unAbs _ = []
    assign :: Ident -> AST
    assign name = AST.Assignment Nothing (accessorString (mkString $ runIdent name) (AST.Var Nothing "this"))
                               (var name)
  valueToJs' (Abs _ arg val) = do
    ret <- valueToJs val
    return $ AST.Function Nothing Nothing [identToJs arg] (AST.Block Nothing [AST.Return Nothing ret])
  valueToJs' e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ AST.Unary Nothing AST.New $ AST.App Nothing (qualifiedToJS id name) args'
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ AST.Unary Nothing AST.New $ AST.App Nothing (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> AST.App Nothing fn [a])) args' <$> valueToJs f
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
  valueToJs' (Case (ss, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs ss binders vals
  valueToJs' (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ AST.App Nothing (AST.Function Nothing Nothing [] (AST.Block Nothing (ds' ++ [AST.Return Nothing ret]))) []
  valueToJs' (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
    return $ AST.VariableIntroduction Nothing (properToJs ctor) (Just $
                AST.ObjectLiteral Nothing [("create",
                  AST.Function Nothing Nothing ["value"]
                    (AST.Block Nothing [AST.Return Nothing $ AST.Var Nothing "value"]))])
  valueToJs' (Constructor _ _ (ProperName ctor) []) =
    return $ iife (properToJs ctor) [ AST.Function Nothing (Just (properToJs ctor)) [] (AST.Block Nothing [])
           , AST.Assignment Nothing (accessorString "value" (AST.Var Nothing (properToJs ctor)))
                (AST.Unary Nothing AST.New $ AST.App Nothing (AST.Var Nothing (properToJs ctor)) []) ]
  valueToJs' (Constructor _ _ (ProperName ctor) fields) =
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

  literalToValueJS :: Literal (Expr Ann) -> m AST
  literalToValueJS (NumericLiteral (Left i)) = return $ AST.NumericLiteral Nothing (Left i)
  literalToValueJS (NumericLiteral (Right n)) = return $ AST.NumericLiteral Nothing (Right n)
  literalToValueJS (StringLiteral s) = return $ AST.StringLiteral Nothing s
  literalToValueJS (CharLiteral c) = return $ AST.StringLiteral Nothing (fromString [c])
  literalToValueJS (BooleanLiteral b) = return $ AST.BooleanLiteral Nothing b
  literalToValueJS (ArrayLiteral xs) = AST.ArrayLiteral Nothing <$> mapM valueToJs xs
  literalToValueJS (ObjectLiteral ps) = AST.ObjectLiteral Nothing <$> mapM (sndM valueToJs) ps

  -- | Shallow copy an object.
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
      evaluate = AST.VariableIntroduction Nothing evaluatedObj (Just obj)
      objAssign = AST.VariableIntroduction Nothing newObj (Just $ AST.ObjectLiteral Nothing [])
      copy = AST.ForIn Nothing key jsEvaluatedObj $ AST.Block Nothing [AST.IfElse Nothing cond assign Nothing]
      cond = AST.App Nothing (accessorString "call" (accessorString "hasOwnProperty" (AST.ObjectLiteral Nothing []))) [jsEvaluatedObj, jsKey]
      assign = AST.Block Nothing [AST.Assignment Nothing (AST.Indexer Nothing jsKey jsNewObj) (AST.Indexer Nothing jsKey jsEvaluatedObj)]
      stToAssign (s, js) = AST.Assignment Nothing (accessorString s jsNewObj) js
      extend = map stToAssign sts
    return $ AST.App Nothing (AST.Function Nothing Nothing [] block) []

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable.
  varToJs :: Qualified Ident -> AST
  varToJs (Qualified Nothing ident) = var ident
  varToJs qual = qualifiedToJS id qual

  -- | Generate code in the simplified JavaScript intermediate representation for a reference to a
  -- variable that may have a qualified name.
  qualifiedToJS :: (a -> Ident) -> Qualified a -> AST
  qualifiedToJS f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = AST.Var Nothing . runIdent $ f a
  qualifiedToJS f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (AST.Var Nothing (moduleNameToJs mn'))
  qualifiedToJS f (Qualified _ a) = AST.Var Nothing $ identToJs (f a)

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

  binderToJs :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs s done binder =
    let (ss, _, _, _) = extractBinderAnn binder in
    traverse (withPos ss) =<< binderToJs' s done binder

  -- | Generate code in the simplified JavaScript intermediate representation for a pattern match
  -- binder.
  binderToJs' :: Text -> [AST] -> Binder Ann -> m [AST]
  binderToJs' _ done NullBinder{} = return done
  binderToJs' varName done (LiteralBinder _ l) =
    literalToBinderJS varName done l
  binderToJs' varName done (VarBinder _ ident) =
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (AST.Var Nothing varName)) : done)
  binderToJs' varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs' varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
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
      return (AST.VariableIntroduction Nothing argVar (Just $ accessorString (mkString $ identToJs field) $ AST.Var Nothing varName) : js)
  binderToJs' _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs' varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (AST.VariableIntroduction Nothing (identToJs ident) (Just (AST.Var Nothing varName)) : js)

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
      return (AST.VariableIntroduction Nothing propVar (Just (accessorString prop (AST.Var Nothing varName))) : js)
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
      return (AST.VariableIntroduction Nothing elVar (Just (AST.Indexer Nothing (AST.NumericLiteral Nothing (Left index)) (AST.Var Nothing varName))) : js)

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
    go js@(AST.NumericLiteral _ (Left i)) =
      let minInt = -2147483648
          maxInt = 2147483647
      in if i < minInt || i > maxInt
         then throwError . errorMessage $ IntOutOfRange i "JavaScript" minInt maxInt
         else return js
    go other = return other
