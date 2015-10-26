-----------------------------------------------------------------------------
--
-- Module      :  Language.PureScript.CodeGen.JS
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  Phil Freeman <paf31@cantab.net>
-- Stability   :  experimental
-- Portability :
--
-- |
-- This module generates code in the simplified Javascript intermediate representation from Purescript code
--
-----------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.CodeGen.JS
  ( module AST
  , module Common
  , moduleToJs
  , mainCall
  ) where

import Data.List ((\\), delete, intersect)
import Data.Maybe (isNothing)
import qualified Data.Traversable as T (traverse)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Arrow ((&&&))
import Control.Monad (replicateM, forM)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.Supply.Class

import Language.PureScript.Crash
import Language.PureScript.AST.SourcePos
import Language.PureScript.CodeGen.JS.AST as AST
import Language.PureScript.CodeGen.JS.Common as Common
import Language.PureScript.CoreFn
import Language.PureScript.Names
import Language.PureScript.CodeGen.JS.Optimizer
import Language.PureScript.Options
import Language.PureScript.Traversals (sndM)
import qualified Language.PureScript.Constants as C

import System.FilePath.Posix ((</>))

-- |
-- Generate code in the simplified Javascript intermediate representation for all declarations in a
-- module.
--
moduleToJs :: forall m. (Applicative m, Monad m, MonadReader Options m, MonadSupply m)
           => Module Ann -> Maybe JS -> m [JS]
moduleToJs (Module coms mn imps exps foreigns decls) foreign_ = do
  jsImports <- T.traverse importToJs . delete (ModuleName [ProperName C.prim]) . (\\ [mn]) $ imps
  jsDecls <- mapM bindToJs decls
  optimized <- T.traverse (T.traverse optimize) jsDecls
  comments <- not <$> asks optionsNoComments
  let strict = JSStringLiteral "use strict"
  let header = if comments && not (null coms) then JSComment coms strict else strict
  let foreign' = [JSVariableIntroduction "$foreign" foreign_ | not $ null foreigns || isNothing foreign_]
  let moduleBody = header : foreign' ++ jsImports ++ concat optimized
  let foreignExps = exps `intersect` (fst `map` foreigns)
  let standardExps = exps \\ foreignExps
  let exps' = JSObjectLiteral $ map (runIdent &&& JSVar . identToJs) standardExps
                             ++ map (runIdent &&& foreignIdent) foreignExps
  return $ moduleBody ++ [JSAssignment (JSAccessor "exports" (JSVar "module")) exps']

  where

  -- |
  -- Generates Javascript code for a module import.
  --
  importToJs :: ModuleName -> m JS
  importToJs mn' = do
    path <- asks optionsRequirePath
    let moduleBody = JSApp (JSVar "require") [JSStringLiteral (maybe id (</>) path $ runModuleName mn')]
    return $ JSVariableIntroduction (moduleNameToJs mn') (Just moduleBody)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a declaration
  --
  bindToJs :: Bind Ann -> m [JS]
  bindToJs (NonRec ident val) = return <$> nonRecToJS ident val
  bindToJs (Rec vals) = forM vals (uncurry nonRecToJS)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a single non-recursive
  -- declaration.
  --
  -- The main purpose of this function is to handle code generation for comments.
  --
  nonRecToJS :: Ident -> Expr Ann -> m JS
  nonRecToJS i e@(extractAnn -> (_, com, _, _)) | not (null com) = do
    withoutComment <- asks optionsNoComments
    if withoutComment
       then nonRecToJS i (modifyAnn removeComments e)
       else JSComment com <$> nonRecToJS i (modifyAnn removeComments e)
  nonRecToJS ident val = do
    js <- valueToJs val
    return $ JSVariableIntroduction (identToJs ident) (Just js)

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a variable based on a
  -- PureScript identifier.
  --
  var :: Ident -> JS
  var = JSVar . identToJs

  -- |
  -- Generate code in the simplified Javascript intermediate representation for an accessor based on
  -- a PureScript identifier. If the name is not valid in Javascript (symbol based, reserved name) an
  -- indexer is returned.
  --
  accessor :: Ident -> JS -> JS
  accessor (Ident prop) = accessorString prop
  accessor (Op op) = JSIndexer (JSStringLiteral op)

  accessorString :: String -> JS -> JS
  accessorString prop | identNeedsEscaping prop = JSIndexer (JSStringLiteral prop)
                      | otherwise = JSAccessor prop

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a value or expression.
  --
  valueToJs :: Expr Ann -> m JS
  valueToJs (Literal _ l) =
    literalToValueJS l
  valueToJs (Var (_, _, _, Just (IsConstructor _ [])) name) =
    return $ JSAccessor "value" $ qualifiedToJS id name
  valueToJs (Var (_, _, _, Just (IsConstructor _ _)) name) =
    return $ JSAccessor "create" $ qualifiedToJS id name
  valueToJs (Accessor _ prop val) =
    accessorString prop <$> valueToJs val
  valueToJs (ObjectUpdate _ o ps) = do
    obj <- valueToJs o
    sts <- mapM (sndM valueToJs) ps
    extendObj obj sts
  valueToJs e@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
    let args = unAbs e
    in return $ JSFunction Nothing (map identToJs args) (JSBlock $ map assign args)
    where
    unAbs :: Expr Ann -> [Ident]
    unAbs (Abs _ arg val) = arg : unAbs val
    unAbs _ = []
    assign :: Ident -> JS
    assign name = JSAssignment (accessorString (runIdent name) (JSVar "this"))
                               (var name)
  valueToJs (Abs _ arg val) = do
    ret <- valueToJs val
    return $ JSFunction Nothing [identToJs arg] (JSBlock [JSReturn ret])
  valueToJs e@App{} = do
    let (f, args) = unApp e []
    args' <- mapM valueToJs args
    case f of
      Var (_, _, _, Just IsNewtype) _ -> return (head args')
      Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      Var (_, _, _, Just IsTypeClassConstructor) name ->
        return $ JSUnary JSNew $ JSApp (qualifiedToJS id name) args'
      _ -> flip (foldl (\fn a -> JSApp fn [a])) args' <$> valueToJs f
    where
    unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
    unApp (App _ val arg) args = unApp val (arg : args)
    unApp other args = (other, args)
  valueToJs (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
    return $ if mn' == mn
             then foreignIdent ident
             else varToJs qi
  valueToJs (Var (_, _, _, Just IsForeign) ident) =
    error $ "Encountered an unqualified reference to a foreign ident " ++ showQualified showIdent ident
  valueToJs (Var _ ident) =
    return $ varToJs ident
  valueToJs (Case (maybeSpan, _, _, _) values binders) = do
    vals <- mapM valueToJs values
    bindersToJs maybeSpan binders vals
  valueToJs (Let _ ds val) = do
    ds' <- concat <$> mapM bindToJs ds
    ret <- valueToJs val
    return $ JSApp (JSFunction Nothing [] (JSBlock (ds' ++ [JSReturn ret]))) []
  valueToJs (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
    return $ JSVariableIntroduction ctor (Just $
                JSObjectLiteral [("create",
                  JSFunction Nothing ["value"]
                    (JSBlock [JSReturn $ JSVar "value"]))])
  valueToJs (Constructor _ _ (ProperName ctor) []) =
    return $ iife ctor [ JSFunction (Just ctor) [] (JSBlock [])
           , JSAssignment (JSAccessor "value" (JSVar ctor))
                (JSUnary JSNew $ JSApp (JSVar ctor) []) ]
  valueToJs (Constructor _ _ (ProperName ctor) fields) =
    let constructor =
          let body = [ JSAssignment (JSAccessor (identToJs f) (JSVar "this")) (var f) | f <- fields ]
          in JSFunction (Just ctor) (identToJs `map` fields) (JSBlock body)
        createFn =
          let body = JSUnary JSNew $ JSApp (JSVar ctor) (var `map` fields)
          in foldr (\f inner -> JSFunction Nothing [identToJs f] (JSBlock [JSReturn inner])) body fields
    in return $ iife ctor [ constructor
                          , JSAssignment (JSAccessor "create" (JSVar ctor)) createFn
                          ]

  iife :: String -> [JS] -> JS
  iife v exprs = JSApp (JSFunction Nothing [] (JSBlock $ exprs ++ [JSReturn $ JSVar v])) []

  literalToValueJS :: Literal (Expr Ann) -> m JS
  literalToValueJS (NumericLiteral n) = return $ JSNumericLiteral n
  literalToValueJS (StringLiteral s) = return $ JSStringLiteral s
  literalToValueJS (CharLiteral c) = return $ JSStringLiteral [c]
  literalToValueJS (BooleanLiteral b) = return $ JSBooleanLiteral b
  literalToValueJS (ArrayLiteral xs) = JSArrayLiteral <$> mapM valueToJs xs
  literalToValueJS (ObjectLiteral ps) = JSObjectLiteral <$> mapM (sndM valueToJs) ps

  -- |
  -- Shallow copy an object.
  --
  extendObj :: JS -> [(String, JS)] -> m JS
  extendObj obj sts = do
    newObj <- freshName
    key <- freshName
    let
      jsKey = JSVar key
      jsNewObj = JSVar newObj
      block = JSBlock (objAssign:copy:extend ++ [JSReturn jsNewObj])
      objAssign = JSVariableIntroduction newObj (Just $ JSObjectLiteral [])
      copy = JSForIn key obj $ JSBlock [JSIfElse cond assign Nothing]
      cond = JSApp (JSAccessor "hasOwnProperty" obj) [jsKey]
      assign = JSBlock [JSAssignment (JSIndexer jsKey jsNewObj) (JSIndexer jsKey obj)]
      stToAssign (s, js) = JSAssignment (JSAccessor s jsNewObj) js
      extend = map stToAssign sts
    return $ JSApp (JSFunction Nothing [] block) []

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
  qualifiedToJS f (Qualified (Just (ModuleName [ProperName mn'])) a) | mn' == C.prim = JSVar . runIdent $ f a
  qualifiedToJS f (Qualified (Just mn') a) | mn /= mn' = accessor (f a) (JSVar (moduleNameToJs mn'))
  qualifiedToJS f (Qualified _ a) = JSVar $ identToJs (f a)

  foreignIdent :: Ident -> JS
  foreignIdent ident = accessorString (runIdent ident) (JSVar "$foreign")

  -- |
  -- Generate code in the simplified Javascript intermediate representation for pattern match binders
  -- and guards.
  --
  bindersToJs :: Maybe SourceSpan -> [CaseAlternative Ann] -> [JS] -> m JS
  bindersToJs maybeSpan binders vals = do
    valNames <- replicateM (length vals) freshName
    let assignments = zipWith JSVariableIntroduction valNames (map Just vals)
    jss <- forM binders $ \(CaseAlternative bs result) -> do
      ret <- guardsToJs result
      go valNames ret bs
    return $ JSApp (JSFunction Nothing [] (JSBlock (assignments ++ concat jss ++ [JSThrow $ failedPatternError valNames])))
                   []
    where
      go :: [String] -> [JS] -> [Binder Ann] -> m [JS]
      go _ done [] = return done
      go (v:vs) done' (b:bs) = do
        done'' <- go vs done' bs
        binderToJs v done'' b
      go _ _ _ = internalError "Invalid arguments to bindersToJs"

      failedPatternError :: [String] -> JS
      failedPatternError names = JSUnary JSNew $ JSApp (JSVar "Error") [JSBinary Add (JSStringLiteral errorMessage) (JSArrayLiteral $ zipWith valueError names vals)]

      errorMessage :: String
      errorMessage = "Failed pattern match" ++ maybe "" (((" at " ++ runModuleName mn ++ " ") ++) . displayStartEndPos) maybeSpan ++ ": "

      valueError :: String -> JS -> JS
      valueError _ l@(JSNumericLiteral _) = l
      valueError _ l@(JSStringLiteral _)  = l
      valueError _ l@(JSBooleanLiteral _) = l
      valueError s _                      = JSAccessor "name" . JSAccessor "constructor" $ JSVar s

      guardsToJs :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m [JS]
      guardsToJs (Left gs) = forM gs $ \(cond, val) -> do
        cond' <- valueToJs cond
        done  <- valueToJs val
        return $ JSIfElse cond' (JSBlock [JSReturn done]) Nothing
      guardsToJs (Right v) = return . JSReturn <$> valueToJs v

  -- |
  -- Generate code in the simplified Javascript intermediate representation for a pattern match
  -- binder.
  --
  binderToJs :: String -> [JS] -> Binder Ann -> m [JS]
  binderToJs _ done (NullBinder{}) = return done
  binderToJs varName done (LiteralBinder _ l) =
    literalToBinderJS varName done l
  binderToJs varName done (VarBinder _ ident) =
    return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : done)
  binderToJs varName done (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [b]) =
    binderToJs varName done b
  binderToJs varName done (ConstructorBinder (_, _, _, Just (IsConstructor ctorType fields)) _ ctor bs) = do
    js <- go (zip fields bs) done
    return $ case ctorType of
      ProductType -> js
      SumType ->
        [JSIfElse (JSInstanceOf (JSVar varName) (qualifiedToJS (Ident . runProperName) ctor))
                  (JSBlock js)
                  Nothing]
    where
    go :: [(Ident, Binder Ann)] -> [JS] -> m [JS]
    go [] done' = return done'
    go ((field, binder) : remain) done' = do
      argVar <- freshName
      done'' <- go remain done'
      js <- binderToJs argVar done'' binder
      return (JSVariableIntroduction argVar (Just (JSAccessor (identToJs field) (JSVar varName))) : js)
  binderToJs _ _ ConstructorBinder{} =
    internalError "binderToJs: Invalid ConstructorBinder in binderToJs"
  binderToJs varName done (NamedBinder _ ident binder) = do
    js <- binderToJs varName done binder
    return (JSVariableIntroduction (identToJs ident) (Just (JSVar varName)) : js)

  literalToBinderJS :: String -> [JS] -> Literal (Binder Ann) -> m [JS]
  literalToBinderJS varName done (NumericLiteral num) =
    return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSNumericLiteral num)) (JSBlock done) Nothing]
  literalToBinderJS varName done (CharLiteral c) =
    return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral [c])) (JSBlock done) Nothing]
  literalToBinderJS varName done (StringLiteral str) =
    return [JSIfElse (JSBinary EqualTo (JSVar varName) (JSStringLiteral str)) (JSBlock done) Nothing]
  literalToBinderJS varName done (BooleanLiteral True) =
    return [JSIfElse (JSVar varName) (JSBlock done) Nothing]
  literalToBinderJS varName done (BooleanLiteral False) =
    return [JSIfElse (JSUnary Not (JSVar varName)) (JSBlock done) Nothing]
  literalToBinderJS varName done (ObjectLiteral bs) = go done bs
    where
    go :: [JS] -> [(String, Binder Ann)] -> m [JS]
    go done' [] = return done'
    go done' ((prop, binder):bs') = do
      propVar <- freshName
      done'' <- go done' bs'
      js <- binderToJs propVar done'' binder
      return (JSVariableIntroduction propVar (Just (accessorString prop (JSVar varName))) : js)
  literalToBinderJS varName done (ArrayLiteral bs) = do
    js <- go done 0 bs
    return [JSIfElse (JSBinary EqualTo (JSAccessor "length" (JSVar varName)) (JSNumericLiteral (Left (fromIntegral $ length bs)))) (JSBlock js) Nothing]
    where
    go :: [JS] -> Integer -> [Binder Ann] -> m [JS]
    go done' _ [] = return done'
    go done' index (binder:bs') = do
      elVar <- freshName
      done'' <- go done' (index + 1) bs'
      js <- binderToJs elVar done'' binder
      return (JSVariableIntroduction elVar (Just (JSIndexer (JSNumericLiteral (Left index)) (JSVar varName))) : js)

mainCall :: ModuleName -> String -> JS
mainCall mmi ns = JSApp (JSAccessor C.main (JSAccessor (moduleNameToJs mmi) (JSVar ns))) []
