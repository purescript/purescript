-- | This module implements the "Magic Do" optimization, which inlines calls to return
-- and bind for the Eff monad, as well as some of its actions.
module Language.PureScript.CoreImp.Optimizer.MagicDo (magicDoEffect, magicDoEff, magicDoST, inlineST) where

import Prelude
import Protolude (ordNub)

import Data.Maybe (fromJust, isJust)

import Language.PureScript.CoreImp.AST (AST(..), InitializerEffects(..), UnaryOperator(..), everything, everywhere, everywhereTopDown)
import Language.PureScript.CoreImp.Optimizer.Common (pattern Ref)
import Language.PureScript.Names (ModuleName)
import Language.PureScript.PSString (mkString)
import Language.PureScript.Constants.Libs qualified as C

-- | Inline type class dictionaries for >>= and return for the Eff monad
--
-- E.g.
--
--  Prelude[">>="](dict)(m1)(function(x) {
--    return ...;
--  })
--
-- becomes
--
--  function __do {
--    var x = m1();
--    ...
--  }
magicDoEff :: (AST -> AST) -> AST -> AST
magicDoEff = magicDo C.M_Control_Monad_Eff C.effDictionaries

magicDoEffect :: (AST -> AST) -> AST -> AST
magicDoEffect = magicDo C.M_Effect C.effectDictionaries

magicDoST :: (AST -> AST) -> AST -> AST
magicDoST = magicDo C.M_Control_Monad_ST_Internal C.stDictionaries

magicDo :: ModuleName -> C.EffectDictionaries -> (AST -> AST) -> AST -> AST
magicDo effectModule C.EffectDictionaries{..} expander = everywhereTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: AST -> AST
  -- Desugar pure
  convert (App _ (App _ pure' [val]) []) | isPure pure' = val
  -- Desugar discard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [] (Block s2 js)]) | isDiscard bind =
    Function s1 (Just fnName) [] $ Block s2 (App s2 m [] : map applyReturns js )
  -- Desugar bind to wildcard
  convert (App _ (App _ bind [m]) [Function s1 Nothing [] (Block s2 js)])
    | isBind bind =
    Function s1 (Just fnName) [] $ Block s2 (App s2 m [] : map applyReturns js )
  -- Desugar bind
  convert (App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 js)]) | isBind bind =
    Function s1 (Just fnName) [] $ Block s2 (VariableIntroduction s2 arg (Just (UnknownEffects, App s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert (App s1 (App _ f [arg]) []) | isEffFunc edUntil f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (Unary s1 Not (App s1 arg [])) (Block s1 []), Return s1 $ ObjectLiteral s1 []])) []
  -- Desugar whileE
  convert (App _ (App _ (App s1 f [arg1]) [arg2]) []) | isEffFunc edWhile f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (App s1 arg1 []) (Block s1 [ App s1 arg2 [] ]), Return s1 $ ObjectLiteral s1 []])) []
  -- Inline __do returns
  convert (Return _ (App _ (Function _ (Just ident) [] body) [])) | ident == fnName = body
  -- Inline double applications
  convert (App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (expander -> App _ (Ref C.P_bind) [Ref dict]) = (effectModule, edBindDict) == dict
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard (expander -> App _ (expander -> App _ (Ref C.P_discard) [Ref C.P_discardUnit]) [Ref dict]) = (effectModule, edBindDict) == dict
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (expander -> App _ (Ref C.P_pure) [Ref dict]) = (effectModule, edApplicativeDict) == dict
  isPure _ = False
  -- Check if an expression represents a function in the Effect module
  isEffFunc name (Ref fn) = (effectModule, name) == fn
  isEffFunc _ _ = False

  applyReturns :: AST -> AST
  applyReturns (Return ss ret) = Return ss (App ss ret [])
  applyReturns (Block ss jss) = Block ss (map applyReturns jss)
  applyReturns (While ss cond js) = While ss cond (applyReturns js)
  applyReturns (For ss v lo hi js) = For ss v lo hi (applyReturns js)
  applyReturns (ForIn ss v xs js) = ForIn ss v xs (applyReturns js)
  applyReturns (IfElse ss cond t f) = IfElse ss cond (applyReturns t) (applyReturns `fmap` f)
  applyReturns other = other

-- | Inline functions in the ST module
inlineST :: AST -> AST
inlineST = everywhere convertBlock
  where
  -- Look for run blocks and inline the STRefs there.
  -- If all STRefs are used in the scope of the same run, only using { read, write, modify } then
  -- we can be more aggressive about inlining, and actually turn STRefs into local variables.
  convertBlock (App s1 (Ref C.P_run) [arg]) =
    let refs = ordNub . findSTRefsIn $ arg
        usages = findAllSTUsagesIn arg
        allUsagesAreLocalVars = all (\u -> let v = toVar u in isJust v && fromJust v `elem` refs) usages
        localVarsDoNotEscape = all (\r -> length (r `appearingIn` arg) == length (filter (\u -> let v = toVar u in v == Just r) usages)) refs
    in App s1 (everywhere (convert (allUsagesAreLocalVars && localVarsDoNotEscape)) arg) []
  convertBlock other = other
  -- Convert a block in a safe way, preserving object wrappers of references,
  -- or in a more aggressive way, turning wrappers into local variables depending on the
  -- agg(ressive) parameter.
  convert agg (App s1 (Ref C.P_new) [arg]) =
   Function s1 Nothing [] (Block s1 [Return s1 $ if agg then arg else ObjectLiteral s1 [(mkString C.stRefValue, arg)]])
  convert agg (App _ (App s1 (Ref C.P_read) [ref]) []) =
    if agg then ref else Indexer s1 (StringLiteral s1 C.stRefValue) ref
  convert agg (App _ (App _ (App s1 (Ref C.P_write) [arg]) [ref]) []) =
    if agg then Assignment s1 ref arg else Assignment s1 (Indexer s1 (StringLiteral s1 C.stRefValue) ref) arg
  convert agg (App _ (App _ (App s1 (Ref C.P_modify) [func]) [ref]) []) =
    if agg then Assignment s1 ref (App s1 func [ref]) else Assignment s1 (Indexer s1 (StringLiteral s1 C.stRefValue) ref) (App s1 func [Indexer s1 (StringLiteral s1 C.stRefValue) ref])
  convert _ other = other
  -- Find all ST Refs initialized in this block
  findSTRefsIn = everything (++) isSTRef
    where
    isSTRef (VariableIntroduction _ ident (Just (_, App _ (App _ (Ref C.P_new) [_]) []))) = [ident]
    isSTRef _ = []
  -- Find all STRefs used as arguments to read, write, modify
  findAllSTUsagesIn = everything (++) isSTUsage
    where
    isSTUsage (App _ (App _ (Ref C.P_read) [ref]) []) = [ref]
    isSTUsage (App _ (App _ (App _ (Ref f) [_]) [ref]) []) | f `elem` [C.P_write, C.P_modify] = [ref]
    isSTUsage _ = []
  -- Find all uses of a variable
  appearingIn ref = everything (++) isVar
    where
    isVar e@(Var _ v) | v == ref = [e]
    isVar _ = []
  -- Convert a AST value to a String if it is a Var
  toVar (Var _ v) = Just v
  toVar _ = Nothing
