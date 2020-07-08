-- |
module Language.PureScript.Sugar.Accessor (desugarAccessorModule) where

import Prelude.Compat

import Data.List (groupBy, concatMap)
import Data.Function (on)
import Data.Monoid (Any(..))
import Control.Monad.Writer

import Language.PureScript.AST
import Language.PureScript.Types
import Language.PureScript.Crash
import Language.PureScript.AST.SourcePos
import Language.PureScript.Externs
import Language.PureScript.Names
import qualified Language.PureScript.Constants as C

-- | Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
desugarAccessorModule :: [ExternsFile] -> Module -> Module
desugarAccessorModule  externs m | not (any (\e -> efModuleName e == ModuleName "Data.Record") externs) = m
desugarAccessorModule _externs (Module ss coms mn ds exts) =
  let (ds', Any used) = runWriter $ traverse desugarAccessor ds
      extraImports =
        if used then 
          addDefaultImport (Qualified (Just C.DataRecord) C.DataRecord)
            . addDefaultImport (Qualified (Just C.DataSymbol) C.DataSymbol)
        else
          id
  in extraImports $ Module ss coms mn ds' exts

-- | Desugar a single let expression
desugarAccessor :: Declaration -> Writer Any Declaration
desugarAccessor decl =
  let (f, _, _) = everywhereOnValuesM pure replace pure
  in f decl
  where
  replace :: Expr -> Writer Any Expr
  replace (Accessor label e) = do
    tell (Any True)
    pure $ App
      (App
        (Var nullSourceSpan C.getField)
        (TypedValue False (Constructor nullSourceSpan C.SProxy)
          (TypeApp nullSourceAnn
            (TypeConstructor nullSourceAnn C.SProxyType)
            (TypeLevelString nullSourceAnn label))))
      e
  replace other = pure other
