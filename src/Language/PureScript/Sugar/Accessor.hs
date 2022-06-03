-- |
module Language.PureScript.Sugar.Accessor
  ( desugarAccessorModule
  ) where

import           Prelude.Compat

import           Control.Monad.Writer

import           Language.PureScript.AST
import qualified Language.PureScript.Constants.Prelude
                                               as C
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           Language.PureScript.Types

-- | Replace every @BoundValueDeclaration@ in @Let@ expressions with @Case@
-- expressions.
desugarAccessorModule :: [ExternsFile] -> Module -> Module
desugarAccessorModule externs m
  | not (any (\e -> efModuleName e == ModuleName "Data.Record") externs) = m
desugarAccessorModule _externs (Module ss coms mn ds exts) =
  let (ds', Any used) = runWriter $ traverse desugarAccessor ds
      extraImports    = if used
        then addDefaultImport (Qualified (Just C.DataRecord) C.DataRecord)
          . addDefaultImport (Qualified (Just C.TypeProxy) C.TypeProxy)
        else id
  in  extraImports $ Module ss coms mn ds' exts

-- | Desugar a single let expression
desugarAccessor :: Declaration -> Writer Any Declaration
desugarAccessor decl =
  let (f, _, _) = everywhereOnValuesM pure replace pure in f decl
 where
  replace :: Expr -> Writer Any Expr
  replace (Accessor label e) = do
    tell (Any True)
    pure $ App
      (App
        (Var nullSourceSpan C.GetField)
        (TypedValue
          False
          (Constructor nullSourceSpan C.Proxy)
          (TypeApp nullSourceAnn
                   (TypeConstructor nullSourceAnn C.ProxyType)
                   (TypeLevelString nullSourceAnn label)
          )
        )
      )
      e
  replace other = pure other
