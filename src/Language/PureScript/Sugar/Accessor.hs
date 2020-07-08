-- |
module Language.PureScript.Sugar.Accessor (desugarAccessorModule) where

import Prelude.Compat

import Data.List (groupBy, concatMap)
import Data.Function (on)

import Language.PureScript.AST
import Language.PureScript.Types
import Language.PureScript.Crash
import Language.PureScript.AST.SourcePos
import qualified Language.PureScript.Constants.Prelude as C

-- | Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
desugarAccessorModule :: Module -> Module
desugarAccessorModule (Module ss coms mn ds exts) = Module ss coms mn (map desugarAccessor ds) exts

-- | Desugar a single let expression
desugarAccessor :: Declaration -> Declaration
desugarAccessor decl =
  let (f, _, _) = everywhereOnValues id replace id
  in f decl
  where
  replace :: Expr -> Expr
  replace (Accessor label e) =
    App
      (App
        (Var nullSourceSpan C.getField)
        (TypedValue False (Constructor nullSourceSpan C.SProxy)
          (TypeApp nullSourceAnn
            (TypeConstructor nullSourceAnn C.SProxyType)
            (TypeLevelString nullSourceAnn label))))
      e
  replace other = other
